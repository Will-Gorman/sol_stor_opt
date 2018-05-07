addprocs()

@everywhere using JuMP, DataFrames, Gurobi, Queryverse

# Set-working directory
#DIR = "C:\\Users\\will-\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
OUT = "out"
INPUT = "in"

# Set constants
LOAD_SHED = [1, 0.75, 0.5, 0.25, 0.1, 0.05, .00001]
BAT_COST = 500 # $/kWh
PV_COST = 3000 # $/kW
BAT_EFF = 0.92
#annual rates
int_rate = 0.06 # percentage interest rate
bat_life = 10 # years
sol_life = 20 # years
BAT_RATE = int_rate / (1 - (1+int_rate)^(-bat_life))
PV_RATE = int_rate / (1 - (1+int_rate)^(-sol_life))
NEM = 0.12
VOS = 0.03

# identify geography IDs to loop through
ID_G = load(DIR * INPUT * "\\id.csv") |> DataFrame

## wrtie optimization function
@everywhere function solar_opt(ID_G, LOAD_SHED, BAT_COST, BAT_RATE, BAT_EFF, PV_COST, PV_RATE, NEM, DIR, INPUT, i)
 
    # set-up data collection objects
    result = DataFrame([Float64,Float64,Float64,String,Float64,Float64,Float64,Float64,Float64], [:pv,:storage,:shed_frac,:id, :load, :ann_cost, :solar_tot,:rfp,:shed_tot], 1)
    outcome = DataFrame([Float64, Float64, Float64,Float64, Float64, Float64,Float64, Float64, String], [:sol,:load,:power_in,:power_out,:shed,:pv_rpf,:bat_chg,:shed_frac,:id], 8760)

    #pick reliability level
    rel_length = length(LOAD_SHED)
    if i % rel_length == 0
        index = rel_length
    else
        index = i % 7 
    end
    shed_amt = LOAD_SHED[index]
        
    #select geography location
    g = ceil(Int, i/7)
    id = string(ID_G[g,1])  

    # Load in solar and load data
    sol = load(DIR * INPUT * "\\res_solar\\" * id * "TYA.CSV.csv") 
    sol = DataFrame(sol)
    sol = sol[1] / 1000  

    load_v = load(DIR * INPUT * "\\BASE\\" * id *".csv") 
    load_v = DataFrame(load_v)
    load_v = load_v[:,2]
    tot_load = sum(load_v)

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
        charging[0], bat_chg[0] == y / 2 ## begining state of charge
        charging2[i=1:8760], bat_chg[i] == bat_chg[i-1] + power_in[i] - power_out[i] ## shows battery cycling
        charging3[i=1:8760], bat_chg[i] <= y ## can't have a higher charge than capacity of battery
        balance[i=1:8760], load_v[i] + power_in[i] + pv_rpf[i] == power_out[i] + (sol[i] * x) + shed[i] ## load balance equation
        powering[i=1:8760], power_in[i] <= y / 4 # rate allowed to charge battery (4 hr battery)
        depowering[i=1:8760], power_out[i] <= y / 4 # rate allowed to discharge battery (4 hr battery)
        shedding[i=1:8760], shed[i] <= load_v[i] ## can't shed load you don't have
        reversing[i=1:8760], pv_rpf[i] <= sol[i] * x ## can't send power back to grid you don't produce
        sum(pv_rpf[i] for i in 1:8760) <= tot_load ## cap on exports
        sum(shed[i] for i in 1:8760) <= tot_load * shed_amt ## reliability constraint
    end)

    @objective(m, Min, x * PV_COST * PV_RATE + y * BAT_COST * BAT_RATE + (sum(shed[i] for i in 1:8760) * NEM) - (sum(pv_rpf[i] for i in 1:8760) * VOS)) 

    status = solve(m)
    
    #optimization solution
    result[1, :pv] = getvalue(x)
    result[1, :storage] = getvalue(y)
    result[1, :shed_frac] = shed_amt
    result[1, :id] = id
    result[1, :load] = tot_load
    result[1, :ann_cost] = getobjectivevalue(m)
    result[1, :solar_tot] = getvalue(x) * sum(sol[i] for i in 1:8760)
    result[1, :rfp] = sum(getvalue(pv_rpf[i]) for i in 1:8760)
    result[1, :shed_tot] = sum(getvalue(shed[i]) for i in 1:8760)
    
    #optimization results
    outcome[1:8760, :sol] = sol * getvalue(x)
    outcome[1:8760, :load] = load_v
    outcome[1:8760, :power_in] = getvalue(power_in)
    outcome[1:8760, :power_out] = getvalue(power_out)
    outcome[1:8760, :shed] = getvalue(shed)
    outcome[1:8760, :pv_rpf] = getvalue(pv_rpf)
    outcome[1:8760, :bat_chg] = getvalue(bat_chg[1:8760])
    outcome[1:8760, :shed_frac] = fill(shed_amt,8760)
    outcome[1:8760, :id] = fill(id,8760)
     
    return status, result, outcome
end

# Create parallelization
timing = @elapsed outputs = pmap(1:length(LOAD_SHED)*length(ID_G[1])) do i #
    solar_opt(ID_G, LOAD_SHED, BAT_COST, BAT_RATE, BAT_EFF, PV_COST, PV_RATE, NEM, DIR, INPUT, i) 
end

#write status
array = outputs |> @map(x->x[1]) |> collect 
df = DataFrame(status = array) 
save(DIR * OUT * "\\status_v5.csv", df) 

#write results table
array = outputs |> @map(x->x[2]) |> collect
for i = 2:length(array) 
    df = array[1]
    df = append!(df, array[i])
end
save(DIR * OUT * "\\results_v5.csv", df)

#write outcome table
array = outputs |> @map(x->x[3]) |> collect
for i = 2:length(array) 
    df = array[1]
    df = append!(df, array[i])
end
save(DIR * OUT * "\\outcome_v5.csv", df)