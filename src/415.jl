


using LifeContingencies
using MortalityTables
using Yields
import LifeContingencies: V, ä 
using DelimitedFiles
using HTTP

AMT = HTTP.get("https://raw.githubusercontent.com/SeanCranston/Pension/main/MortalityTables/23AMT.csv").body
AMT = readdlm("https://raw.githubusercontent.com/SeanCranston/Pension/main/MortalityTables/23AMT.csv", ','); 
ult = UltimateMortality(convert(Vector{Float64},AMT[2:121,1]),start_age = 1);
# Base limit lookup for a given year
function base_415_limit(year::Int)
    limits = Dict(2022 => 245000, 2023 => 265000)  # Add more years as needed
    return get(limits, year, 265000)  # Default to $265,000 if year is missing
end

# Early Retirement Adjustment (if retiring before age 62)
function early_retirement_adjustment(limit::Float64, age::Int)
    if age >= 62
        return limit
    else
        reduction_factor = (1 - 0.05) ^ (62 - age)  # 5% reduction per year below 62
        return limit * reduction_factor
    end
end

# Late Retirement Adjustment Using IRS 5% Interest Rate
function late_retirement_adjustment(limit::Float64, age::Int)
    if age <= 65
        return limit
    else
        actuarial_factor = (1 + 0.05) ^ (age - 65)  # 5% increase per year after 65
        return limit * actuarial_factor
    end
end

# Main function to compute the 415 limit for defined benefit plans
function defined_benefit_415_limit(age::Int, year::Int)
    # Step 1: Get the base limit for the year
    limit = base_415_limit(year)

    # Step 2: Apply early retirement reduction if applicable
    limit = early_retirement_adjustment(limit, age)

    # Step 3: Apply late retirement increase if applicable
    limit = late_retirement_adjustment(limit, age)

    return limit
end

# Example: Calculate the 415 limit for someone retiring at age 67 in 2023
age = 67
year = 2023
limit = defined_benefit_415_limit(age, year)
println("The 415 limit for a $age-year-old participant in $year is: \$", limit)


# can I make a plan specs structure like how SinlgeLife is make????

function FutureService(NRA_Service::Float64, NRA::Float64, Age::Float64)
    return max(
        0,
        max(
            NRA_Service - 1 - 2022 + 2022,
            NRA - Age
        )
    );
end

function FutureService(NRA_Service::Int64, NRA::Int64, Age::Float64)
    return max(
        0,
        max(
            NRA_Service - 1 - 2022 + 2022,
            NRA - Age
        )
    );
end

#FutureService(NRA_Service, NRA, Age)

# 'actuarial objects' that combine multiple forms of Limits set by the IRS for retirment Plans
abstract type IRS_Limits end

struct IRS_2022{} <: IRS_Limits
    Dollar::Int
    Comp::Int
end

function IRS_2022(;Dollar, Comp) 
    return IRS_2022(Dollar, Comp)
end


# 'actuarial objects' that combine multiple forms of plan specs (DB, CB, DC)
abstract type PlanSpecs end

"""
struct DB_PlanSpecs{M,D} <: PlanSpecs
    mortality::M
    issue_age::Int
    alive::Bool
    fractional_assump::D
end


A `PlanSpecs` object containing the necessary assumptions for contingent maths related to a Retirement Plans. Use with a `DB_PlanSpecs` to do many actuarial present value calculations. 
Keyword arguments:
- `mortality` pass a mortality vector, which is an array of applicable mortality rates indexed by attained age
- `issue_age` is the assumed issue age for the `SingleLife` and is the basis of many contingency calculations.
- `alive` Default value is `true`. Useful for joint insurances with different status on the lives insured.
- `fractional_assump`. Default value is `Uniform()`. This is a `DeathDistribution` from the `MortalityTables.jl` package and is the assumption to use for non-integer ages/times.


# Examples
    using MortalityTables
    mortality = MortalityTables.table("2001 VBT Residual Standard Select and Ultimate - Male Nonsmoker, ANB")
    SingleLife(
        mort       = mort.select[30], 
        issue_age  = 30          
    )

    
 PS = DB_PlanSpecs(age = 58.28, val_year = 2022, mortality = ult, AE = 0.05, ICR = 0.05, GATT = 0.05, NRA = 62, NRA_Service = 0, AgeTest= 65, IRS = 245000)

 This shows that this method only differs from DBVAL by roughly 0.04526209%
 (Max_LS(PS) - 3151035.55)/3151035.55 * 100

"""

struct DB_PlanSpecs{M,IRS_Limits} <: PlanSpecs
    age::Float64
    val_year::Int
    mortality::M
    AE::Float64
    ICR::Float64
    GATT::Float64
    NRA::Int
    NRA_Service::Int
    AgeTest::Int
    IRS::IRS_Limits
end

function DB_PlanSpecs(;age, val_year, mortality, AE, ICR, GATT, NRA, NRA_Service, AgeTest, IRS)
    return DB_PlanSpecs(age, val_year, mortality, AE, ICR, GATT, NRA, NRA_Service, AgeTest, IRS)
end

struct DB_Benefit <: PlanSpecs
    AccrualMethod::String
    Comp::Float64
    PartialComp::Float64
    PartialCompBool::Bool
    ContributionRate::Float64
    MinAccrual::Float64
    MinAccrualRate::Float64
end

# PS = DB_Benefit(;AccrualMethod = "Accrual", Comp = 61000.0, PartialComp=0.0, PartialCompBool = true, ContributionRate = 1.00)

function DB_Benefit(;AccrualMethod, Comp, PartialComp, PartialCompBool, ContributionRate)
    return DB_Benefit(AccrualMethod, Comp, PartialComp, PartialCompBool, ContributionRate)
end



"""
From DBVAL we have the formula

MAX(
    IF(V11+W11+CN11>65, #Age Plus FUT Srv is greater than 65 then
        
        MIN(
            # (1+ GATT)^(Age + FutureService(NRA_Service, NRA, Age) - 65) * ä(LifeContingency(life = SingleLife(mortality = ult, issue_age = 65), Yields.Constant(AE)), frequency = 12) / ä(LifeContingency(life = SingleLife(mortality = ult, issue_age = Age + FutureService(NRA_Service, NRA, Age)), Yields.Constant(AE)), frequency = 12)
            (1+'Plan Specs'!\$D$10*0+'Plan Specs'!\$D$13*1)^(V11+W11+CN11-65)*VLOOKUP(65,ANNUITY!\$DP$14:\DW$134,8,FALSE)/VLOOKUP(ROUND(V11+W11+CN11,0),ANNUITY!\$DP$14:\$DW$134,8,FALSE),
            # 1.05^(Age_FtSrv-65)*ä(Testlc, frequency = 12)/ä(lc, frequency = 12)* Limit_IRS415/10 * ä(lc5_5, frequency = 12)
            1.05^(V11+W11+CN11-65)*VLOOKUP(65,ANNUITY!\$DM$14:\$DN$134,2,FALSE)/VLOOKUP(ROUND(V11+W11+CN11,0),ANNUITY!\$DM$14:\$DN$134,2,FALSE))
                *'Plan Specs'!\$D$63/10*VLOOKUP(ROUND(V11+W11+CN11,0),ANNUITY!\$AE$14:\$AL$134,8,FALSE),
        
        IF(V11+W11+CN11<62, #if Age + FUT SRV is less than 65 and less then 62 then
        
            MIN(
                # (1 + GATT)^(Age_FtSrv - 62) * ä(Testlc2, frequency = 12) /  ä(lc, frequency = 12)
                (1+'Plan Specs'!\$D$10*0+'Plan Specs'!\$D$13*1)^(V11+W11+CN11-62)*VLOOKUP(62,ANNUITY!\$DP$14:\$DW$134,8,FALSE)/VLOOKUP(ROUND(V11+W11+CN11,0),ANNUITY!\$DP$14:\$DW$134,8,FALSE),
                # 1.05^(Age_FtSrv-62)*ä(Testlc2, frequency = 12) / ä(lc, frequency = 12)
                1.05^(V11+W11+CN11-62)*VLOOKUP(62,ANNUITY!\$DM$14:\$DN$134,2,FALSE)/VLOOKUP(ROUND(V11+W11+CN11,0),ANNUITY!\$DM$14:\$DN$134,2,FALSE))
                    # * Limit_IRS415 / 10 * ä(lc5_5, frequency = 12)
                    *'Plan Specs'!\$D$63/10*VLOOKUP(ROUND(V11+W11+CN11,0),ANNUITY!\$AE$14:\$AL$134,8,FALSE),
                    
            #If 62 <= Age + FUT SRV <= 65 then
            'Plan Specs'!\$D$63/10*VLOOKUP(ROUND(V11+W11+CN11,0),ANNUITY!\$AE$14:\$AL$134,8,FALSE)))*10,
            
    'Plan Specs'!\$C$41*VLOOKUP(ROUND(V11+W11+CN11,0),ANNUITY!\$AE$14:\$AL$134,8,FALSE)
)# Not this one
"""

function Dollar_Limit(PS::DB_PlanSpecs)
    # Need to add Early and late retirement factor

    #Find the smallest annuity out of plan vs statuetory assumptions
    # Plan specs assumptions
    Testlife = SingleLife(PS.mortality, issue_age = PS.AgeTest);
    TestlcGATT = LifeContingency(Testlife, Yields.Constant(PS.GATT));

    # statuetory assumptions
    IRSlife = SingleLife(ult, issue_age = PS.AgeTest);
    IRS_GATT = LifeContingency(IRSlife, Yields.Constant(.055));

    APR = min(ä(TestlcGATT, frequency = 12), ä(IRS_GATT, frequency = 12))

    return round(PS.IRS * APR, digits = 2)

end

function Comp_Limit(PS::DB_PlanSpecs)
    #Find the smallest annuity out of plan vs statuetory assumptions
    # Plan specs assumptions
    Testlife = SingleLife(PS.mortality, issue_age = PS.AgeTest);
    TestlcGATT = LifeContingency(Testlife, Yields.Constant(PS.GATT));

    # statuetory assumptions
    IRSlife = SingleLife(ult, issue_age = PS.AgeTest);
    IRS_GATT = LifeContingency(IRSlife, Yields.Constant(.055));

    APR = min(ä(TestlcGATT, frequency = 12), ä(IRS_GATT, frequency = 12))

    #Need to input average compensation
    return round(PS.comp * APR, digits = 2)

end

function Max_LS(PS::DB_PlanSpecs)
    # Testing (conservative) assumptions - statue assumptions 5.5% and AMT
    Testlife = SingleLife(PS.mortality, issue_age = PS.AgeTest);
    TestlcGATT = LifeContingency(Testlife, Yields.Constant(PS.GATT));
    Testlc = LifeContingency(Testlife, Yields.Constant(PS.AE));
    Testlife2 = SingleLife(PS.mortality, issue_age = 62);
    TestlcGATT2 = LifeContingency(Testlife2, Yields.Constant(PS.GATT));
    Testlc2 = LifeContingency(Testlife2, Yields.Constant(PS.AE));

    # Plan specs assumptions
    Age_FtSrv = PS.age + FutureService(PS.NRA_Service, PS.NRA, PS.age);
    life =  SingleLife(PS.mortality, issue_age = 62); # Age_FtSrv
    lcGATT = LifeContingency(life, Yields.Constant(PS.GATT));
    lc5_5 = LifeContingency(life, Yields.Constant(0.055));
    lc = LifeContingency(life, Yields.Constant(PS.AE));

    if PS.age + FutureService(PS.NRA_Service, PS.NRA, PS.age) > 65
        temp = min(
            (1+ PS.GATT)^(Age_FtSrv - PS.AgeTest) * ä(Testlc, frequency = 12) / ä(lc, frequency = 12),
            1.05^(Age_FtSrv-65) * ä(Testlc, frequency = 12) / ä(lc, frequency = 12)
        ) * PS.IRS / 10 * ä(lc5_5, frequency = 12)
    elseif PS.age + FutureService(PS.NRA_Service, PS.NRA, PS.age) < 62
        temp = min(
            (1 + PS.GATT)^(Age_FtSrv - 62) * ä(Testlc2, frequency = 12) /  ä(lc, frequency = 12),
            1.05^(Age_FtSrv-62) * ä(Testlc2, frequency = 12) / ä(lc, frequency = 12)
        ) * PS.IRS / 10 * ä(lc5_5, frequency = 12)
    else
        temp = PS.IRS / 10 * ä(lc5_5, frequency = 12) * 10
    end

    return max(temp, 0) #not sure what this is pointing to
end


'
Define min Accrual


'




"""
Define some Allocation Methods

"""

# CBP rate is determined by Compensation (eligible)

#PS = DB_PlanSpecs(age = 58.28, val_year = 2024, mortality = ult, AE = 0.05, ICR = 0.05, GATT = 0.05, NRA = 62, NRA_Service = 0, AgeTest= 62, IRS = 275000)

# PS = DB_Benefit(AccrualMethod = "415", Comp = 61000.0, PartialComp=0.0, PartialCompBool = true, ContributionRate = 1.00)

# Allocation(AccrualMethod = "Accrual", PS = PS)


function Allocation(PS::DB_Benefit, IRS::IRS_2022)
    if PS.AccrualMethod == "Comp"
        return round(PS.ContributionRate * max(PS.Comp, PS.PartialComp), RoundUp, digits = 2)
    elseif PS.AccrualMethod == "415"
        return round(PS.ContributionRate * Max_LS(DB_PlanSpecs), RoundUp, digits = 2)
    elseif PS.AccrualMethod == "Accrual"
        return round(PS.ContributionRate * PS.MinAccrual / PS.MinAccrualRate, RoundUp, digits = 2) # Need to define MinAccrual
    elseif PS.AccrualMethod == "Excess"
        return round(min(IRS.Dollar, max(0,PS.Comp - ContributionRate)) * PS.ContributionRate, RoundUp, digits = 2)
    else # Flat 
        return round(PS.ContributionRate, RoundUp, digits = 2)
    end
end



"""
Now we define some liabilities

"""

abstract type liability end
#abstract type rates end


"""
This section will define a plans Funding Target.

Below is DBVal for Funding Target

=(U15-T15)* (1+IF(O15-M15>4.99,IF(O15-M15>19.99,\$K$7,\$J$7),\$I$7))^(M15-O15) *(1+\$H$8)^MIN(1,(O15-M15))*(1+\$H$9)^MAX(0,O15-M15-1) *IF(G15="T",AY15,1)


Examples :
    Segs = SegmentRates(Seg1 = 0.0475, Seg2 = 0.0518, Seg3 = 0.0592);
    E = EE(Age = 58, Allocation = 0, EndBal = 61000);
    PS = DB_PlanSpecs(age = 58.0, val_year = 2022, mortality = ult, AE = 0.05, ICR = 0.05, GATT = 0.05, NRA = 62, NRA_Service = 0, AgeTest= 65, IRS = 245000);

    FundingTarget(Segs, E, PS)

"""

struct SegmentRates{}
    Seg1::Float32 # for a participant that has < 5 years before Retirement
    Seg2::Float32 # for a participant that has >= 5 years before Retirement but less than 20
    Seg3::Float32 # for a participant that has >= 20 years before Retirement
end

function SegmentRates(;Seg1, Seg2, Seg3)
    return SegmentRates(Seg1, Seg2, Seg3)
end

# Should we have ee with data that doesn't change and ee with data that can change?
struct EE{}
    DOB::Date
    DOH::Date
    DOP::Date
    Gender::String
    Div::Int
    Age::Int
    Allocation::Int
    EndBal::Int
    High3::Float64
    CompFixed::Float64
    CompVar::Float64
    Hours::Float64
end

function EE(;Age, Allocation, EndBal)
    return EE(Age, Allocation, EndBal)
end

struct FundingTarget{SegmentRates, EE, DB_PlanSpecs} <: liability
    Segs::SegmentRates
    EE::EE
    PS::DB_PlanSpecs
end


function FundingTarget(Segs::SegmentRates, E::EE, PS::DB_PlanSpecs)
    if PS.NRA - PS.age >= 20
        Seg = Segs.Seg3
    elseif PS.NRA - PS.age >= 5
        Seg = Segs.Seg2
    else
        Seg = Segs.Seg1
    end
    Funding = E.EndBal - E.Allocation;
    Discount = (1+Seg)^(PS.age - PS.NRA);
    Project = (1 + PS.AE)^min(1,PS.NRA - PS.age)*0 + (1 + PS.AE)^max(0,PS.NRA - PS.age); # EA pre and post retirement age
    Vesting = 1;
    return  round(Funding * Discount * Project * Vesting, digits = 2) 
end

'
Segs = SegmentRates(Seg1 = 0.0127, Seg2 = 0.0299, Seg3 = 0.0351);
E = EE(Age = 58, Allocation = 0, EndBal = 61000);
PS = DB_PlanSpecs(age = 58.0, val_year = 2022, mortality = ult, AE = 0.05, ICR = 0.05, GATT = 0.05, NRA = 62, NRA_Service = 0, AgeTest= 65, IRS = 245000);

FundingTargetMax(Segs, E, PS)

'

function FundingTargetMax(Segs::SegmentRates, E::EE, PS::DB_PlanSpecs)
    if PS.NRA - PS.age >= 20
        Seg = Segs.Seg3
    elseif PS.NRA - PS.age >= 5
        Seg = Segs.Seg2
    else
        Seg = Segs.Seg1
    end
    Funding = E.EndBal - E.Allocation;
    Discount = (1+Seg)^(PS.age - PS.NRA);
    Project = (1 + PS.AE)^min(1,PS.NRA - PS.age)*0 + (1 + PS.AE)^max(0,PS.NRA - PS.age); # EA pre and post retirement age
    Vesting = 1;
    return  round(Funding * Discount * Project * Vesting, digits = 2) 
end


'
Target Normal Cost is the value of the allocation given at NRA projected back to AA with the respective seg rate (max) and then projected back to NRA. 
That is TNC 

Segs = SegmentRates(Seg1 = 0.0127, Seg2 = 0.0299, Seg3 = 0.0351);
E = EE(Age = 58, Allocation = 0, EndBal = 0); #61000
PS = DB_PlanSpecs(age = 58.0, val_year = 2022, mortality = ult, AE = 0.05, ICR = 0.05, GATT = 0.05, NRA = 62, NRA_Service = 0, AgeTest= 65, IRS = 245000);

TargetNormalCost(Segs, E, PS)
'

function TargetNormalCost(Segs::SegmentRates, E::EE, PS::DB_PlanSpecs)
    if PS.NRA - PS.age >= 20
        Seg = Segs.Seg3
    elseif PS.NRA - PS.age >= 5
        Seg = Segs.Seg2
    else
        Seg = Segs.Seg1
    end
    Balance = E.EndBal
    Funding = E.EndBal - E.Allocation;
    Discount = (1+Seg)^(PS.age - PS.NRA);
    Project = (1 + PS.AE)^min(1,PS.NRA - PS.age) + (1 + PS.AE)^max(0,PS.NRA - PS.age); # EA pre and post retirement age
    Vesting = 1;
    FT = round(Funding * Discount * Project * Vesting, digits = 2) 
    return  round(Balance * Discount * Project * Vesting - FT, digits = 2) 
end


"""
Cushion:
FT = FundingTargetMax(Segs, E, PS)
IncreaseFT = 0
IncreaseSalary = 0
Cushion(FT, IncreaseFT, IncreaseSalary)
"""

function Cushion(FT, IncreaseFT, IncreaseSalary)
    return round(0.5(FT - IncreaseFT) + IncreaseSalary, digits = 2)
end

'

MaxDeductible(Cushion(FundingTargetMax(Segs, E, PS)), TargetNormalCost(Segs, E, PS), FundingTargetMax(Segs, E, PS))

'

function MaxDeductible(Cushion, TNC, FT)
    return round(Cushion + TNC + FT, RoundDown, digits = -3)
end


'
MinDeductible(TargetNormalCost(Segs, E, PS), 0, 0)
'


function MinDeductible(TNC, ExcessAssets, AmortInstall)
    return max(0, round(TNC + ExcessAssets + AmortInstall, RoundUp, digits = -3))
end
