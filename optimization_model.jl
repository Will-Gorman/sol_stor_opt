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
id_g = convert(Array,CSV.read(DIR * INPUT * "\\id_test.csv"))

# set-up data collection objects
result = DataFrame(pv=Float64[],storage=Float64[], shed_frac=Float64[], id=String[])
variable_outcome = DataFrame(power_in=Float64[],power_out=Float64[], shed=Float64[], pv_rpf=Float64[],bat_chg=Float64[], shed_frac=Float64[], id=String[])


for j = 1:length(LOAD_SHED)

    #pick reliability level
    shed_amt = LOAD_SHED[j]

    for g = 1:length(id_g)
        
        # set-up temporary data collection objects
        temp_result = DataFrame(pv=Float64[],storage=Float64[], shed_frac=Float64[], id=String[])
        temp_outcome = DataFrame(power_in=Float64[],power_out=Float64[], shed=Float64[], pv_rpf=Float64[],bat_chg=Float64[], shed_frac=Float64[], id=String[])

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
        temp_result[:pv] = getvalue(x)
        temp_result[:storage] = getvalue(y)
        temp_result[:shed_frac] = shed_amt
        temp_result[:id] = id
        append!(result, temp_result)

        #optimization results
        temp_outcome[:power_in] = getvalue(power_in)
        temp_outcome[:power_out] = getvalue(power_out)
        temp_outcome[:shed] = getvalue(shed)
        temp_outcome[:pv_rpf] = getvalue(pv_rpf)
        temp_outcome[:bat_chg] = getvalue(bat_chg[1:8760])
        temp_outcome[:shed_frac] = shed_amt
        temp_outcome[:id] = id
        append!(variable_outcome, temp_outcome)

    end
end

#Write out results
CSV.write(DIR * OUT * "\\opt_result.csv", result)
CSV.write((DIR * OUT * "\\variable_outcome.csv"), variable_outcome)
