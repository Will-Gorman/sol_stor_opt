using CSV, JuMP, Ipopt, ConditionalJuMP

# Define Constants
DIR = "C:\\Users\\Will\\GoogleDrive\\UCBerkeley\\2018Spring\\Comp Programing in Econ\\Project\\"
OUT = "out"
INPUT = "in"
BAT_COST = 200
BAT_RATE = 0.12
BAT_EFF = 0.92
PV_COST = 1500
PV_RATE = 0.07
LOAD_SHED = 0.01
NEM = 0.12

# Load in solar and load data
sol = convert(Array,CSV.read(DIR * INPUT * "\\res_solar\\690150TYA.CSV.csv")) / 1000
load = CSV.read(DIR * INPUT * "\\BASE\\690150.csv")
load = load[:,2]
tot_load = sum(load)

## write conditional functions
function input(x)
    @ifelse(x < 0, 
      0,
      x
      )
end

function solar(x)
    @ifelse(x == 0, 
      0,
      x
      )
end

# Set-up optimization model
m = Model(solver = IpoptSolver()) # specify the solver, pass along to model

@variables m begin
    x >= 0 ## solar capacity
    y >= 0 ## storage capacity
    y >= bat_chg[0:8760] >= 0. ## battery state of charge
    ex_load[1:8760] >= 0. ## excess load pre-battery
    ex_pv[1:8760] >= 0. ## excess pv pre-battery
    shed[1:8760] >= 0. ## total load shed with battery
    pv_rpf[1:8760] >= 0.  ## total extra pv with battery
end

@constraints(m, begin
    bat_chg[0] == y
    loading[i=1:8760], ex_load[i] == input((load[i] - sol[i] * x)) ## if these are defined outside of the model, will it work? 
    solar[i=1:8760], ex_pv[i] == input((sol[i] * x - load[i]))
    reverse[i=1:8760], pv_rpf[i] == solar(ex_pv[i] - (bat_chg[i] - bat_chg[i-1]) / BAT_EFF)
    charging[i=1:8760], bat_chg[i] == bat_chg[i-1] + ex_pv[i] * y - ex_load[i]
    shed_load[i=1:8760], shed[i] == bat_chg[i] - bat_chg[i-1] - ex_load[i]
    sum(shed[i] for i:8760) <= tot_load * LOAD_SHED
end)

@objective(m, Min, x * PV_COST * PV_RATE + y * BAT_COST * BAT_RATE + (sum(shed[i] for i:8760) - sum(pv_rpf[i] for i:8760)) * NEM) 

solve(m)
getvalue(x)