using CSV, JuMP, Gurobi #CPLEX

# Define Constants
#DIR = "C:\\Users\\will-\\Google Drive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
OUT = "out"
INPUT = "in"
BAT_COST = 200
BAT_RATE = 0.12
BAT_EFF = 0.92
PV_COST = 1500
PV_RATE = 0.07
LOAD_SHED = 0.0000001
NEM = 0.12

# Load in solar and load data
sol = convert(Array,CSV.read(DIR * INPUT * "\\res_solar\\690150TYA.CSV.csv")) / 1000
load = CSV.read(DIR * INPUT * "\\BASE\\690150.csv")
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
    sum(shed[i] for i in 1:8760) <= tot_load * LOAD_SHED
end)

@objective(m, Min, x * PV_COST * PV_RATE + y * BAT_COST * BAT_RATE + (sum(shed[i] for i in 1:8760) * NEM)) 

solve(m)
getvalue(x)
getvalue(y)