using CSV, JuMP, DataFrames, Gurobi #Gurobi #CPLEX

# Set-working directory
#DIR = "C:\\Users\\will-\\Google Drive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
OUT = "out"
INPUT = "in"

# Set constants
BAT_COST = 200
BAT_RATE = 0.12
BAT_EFF = 0.92
PV_COST = 1500
PV_RATE = 0.07
LOAD_SHED = [1, 0.75, 0.5, 0.25, 0.1, 0.05, .00001]
NEM = 0.12

# identify geography IDs to loop through
id_g = convert(Array,CSV.read(DIR * INPUT * "\\id.csv"))

# set-up data collection objects
result = DataFrame([Float64,Float64, Float64, String], [:pv,:storage,:shed_frac,:id], length(LOAD_SHED) * length(id_g))
outcome = DataFrame([Float64,Float64, Float64, Float64,Float64, Float64, String], [:power_in,:power_out,:shed,:pv_rpf,:bat_chg,:shed_frac,:id],length(LOAD_SHED) * length(id_g)*8760)

row = 1

for j = 1:length(LOAD_SHED)

    #pick reliability level
    shed_amt = LOAD_SHED[j]

    for g = 1:length(id_g)
        
        
        #select geography location
        id = string(id_g[g])
    
        # Load in solar and load data
        sol = convert(Array,CSV.read(DIR * INPUT * "\\res_solar\\" * id * "TYA.CSV.csv")) / 1000
        load = CSV.read(DIR * INPUT * "\\BASE\\" * id *".csv")
        load = load[:,2]
        tot_load = sum(load)

        # Set-up optimization model
        m = Model(solver = GurobiSolver()) # specify the solver, pass along to model 	CplexSolver() GurobiSolver()

        @variables m begin
            x >= 0. ## solar capacity
            y >= 0. ## storage capacity
            #state[i=1:8760], Int ## if discharging or charging
            power_in[i=1:8760] >= 0. ## charging amount
            power_out[i=1:8760] >= 0. ## discharging amount
            bat_chg[i=0:8760] >= 0. ## battery state of charge
            shed[i=1:8760] >= 0. ## total load shed with battery
            pv_rpf[i=1:8760] >= 0.  ## total extra pv with battery
        end

        @constraints(m, begin
            charging[0], bat_chg[0] == y / 2
            charging2[i=1:8760], bat_chg[i] == bat_chg[i-1] + power_in[i] - power_out[i]
            charging3[i=1:8760], bat_chg[i] <= y 
            balance[i=1:8760], load[i] + power_in[i] + pv_rpf[i] == power_out[i] + (sol[i] * x) + shed[i] 
            powering[i=1:8760], power_in[i] <= y #* state[i]
            depowering[i=1:8760], power_out[i] <= y #* (1-state[i])
            bat_power1[i=1:8760], power_in[i] + bat_chg[i] <= y
            bat_power2[i=1:8760], bat_chg[i] - power_out[i] >= 0 
            sum(shed[i] for i in 1:8760) <= tot_load * shed_amt
        end)

        @objective(m, Min, x * PV_COST * PV_RATE + y * BAT_COST * BAT_RATE + (sum(shed[i] for i in 1:8760) * NEM)) 

        solve(m)

        #optimization solution
        result[row, :pv] = getvalue(x)
        result[row, :storage] = getvalue(y)
        result[row, :shed_frac] = shed_amt
        result[row, :id] = id
        

        #optimization results
        outcome[(1+8760*(row - 1)):8760*row, :power_in] = getvalue(power_in)
        outcome[(1+8760*(row - 1)):8760*row, :power_out] = getvalue(power_out)
        outcome[(1+8760*(row - 1)):8760*row, :shed] = getvalue(shed)
        outcome[(1+8760*(row - 1)):8760*row, :pv_rpf] = getvalue(pv_rpf)
        outcome[(1+8760*(row - 1)):8760*row, :bat_chg] = getvalue(bat_chg[1:8760])
        outcome[(1+8760*(row - 1)):8760*row, :shed_frac] = fill(shed_amt,8760)
        outcome[(1+8760*(row - 1)):8760*row, :id] = fill(id,8760)
        
        row +=1

    end
end

#Write out results
CSV.write(DIR * OUT * "\\opt_result.csv", result)
CSV.write((DIR * OUT * "\\variable_outcome.csv"), outcome)
