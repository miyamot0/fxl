##
## Copyright 2021 Shawn Gilroy
##
## This file is part of fxl.
##
## fxl is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 2.
##
## fxl is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with fxl  If not, see <http://www.gnu.org/licenses/gpl-2.0.html>.

#' @title Plotting data from Gilroy et al. (2015)
#'
#' @description Plotting data from Gilroy et al. (2015)
#'
#' @format A data frame with 40 rows and 6 variables:
#' \describe{
#'   \item{Participant}{Participant name}
#'   \item{Session}{Session number}
#'   \item{Condition}{Condition name}
#'   \item{Responding}{Responding rates}
#'   \item{PhaseNum}{Phase number}
#'   \item{LineOff}{Offset for phase line}
#' }
#' @source <doi:https://doi.org/10.1016/j.rasd.2015.04.004>
"Gilroyetal2015"

#' @title Plotting data from Gilroy et al. (2019) - FA
#'
#' @description FA data from Gilroy et al. (2019)
#'
#' @format A data frame with 15 rows and 9 variables:
#' \describe{
#'   \item{Session}{Session number}
#'   \item{SIB}{Rates of self-injury}
#'   \item{AGG}{Rates of aggression}
#'   \item{DIS}{Rates of disruptive behavior}
#'   \item{Prompt}{Rates of prompting}
#'   \item{Comply}{Rates of compliance}
#'   \item{SR}{Duration of reinforcement}
#'   \item{CTB}{Rates of combined target behavior}
#'   \item{Condition}{Functional analysis condition}
#' }
#' @source <doi:https://doi.org/10.1080/17518423.2019.1646342>
"Gilroyetal2019"

#' @title Plotting data from Gilroy et al. (2019) - Treatment
#'
#' @description Treatment data from Gilroy et al. (2019)
#'
#' @format A data frame with 86 rows and 8 variables:
#' \describe{
#'   \item{Participant}{Participant name}
#'   \item{Session}{Session number}
#'   \item{Condition}{Functional analysis condition}
#'   \item{CTB}{Rates of combined target behavior}
#'   \item{FCR}{Rates for communication response for function 1}
#'   \item{FCR2}{Rates for communication response for function 2}
#'   \item{PhaseNum}{Sequenced phase number}
#'   \item{LineOff}{Offset of phase line}
#' }
#' @source <doi:https://doi.org/10.1080/17518423.2019.1646342>
"Gilroyetal2019Tx"

#' @title Plotting data from Gilroy et al. (2015) - Treatment
#'
#' @description Treatment data from Gilroy et al. (2021)
#'
#' @format A data frame with 69 rows and 7 variables:
#' \describe{
#'   \item{Participant}{Participant name}
#'   \item{Session}{Session number}
#'   \item{Condition}{Functional analysis condition}
#'   \item{Responding}{Rates of responding}
#'   \item{Reinforcers}{Reinforcer deliveries}
#'   \item{PhaseNum}{Sequenced phase number}
#'   \item{LineOff}{Offset of phase line}
#' }
#' @source <doi:https://doi.org/10.1002/jaba.826>
"Gilroyetal2021"

#' @title Plotting data from Lozy et al. (2020)
#'
#' @description Treatment data from Lozy et al. (2020)
#'
#' @format A data frame with 91 rows and 5 variables:
#' \describe{
#'   \item{Session}{Session number}
#'   \item{Participant}{Participant name}
#'   \item{KM}{Kinesthetic movement choices}
#'   \item{TD}{Traditional drill choices}
#'   \item{Phase}{Sequenced phase number}
#' }
#' @source <doi:https://doi.org/10.1002/jaba.677>
"LozyEtAl2020"

#' @title Plotting data from Koffarnus et al. (2011)
#'
#' @description Treatment data from Koffarnus et al. (2011)
#'
#' @format A data frame with 14979 rows and 3 variables:
#' \describe{
#'   \item{X}{Session/day number}
#'   \item{ID}{Participant ID on the Y axis}
#'   \item{Code}{Status for treatment}
#' }
#' @source <doi:https://doi.org/10.1093/alcalc/agr057>
"KoffarnusEtAl2011"

#' @title Plotting data from Koffarnus et al. (2011)
#'
#' @description Treatment data from Koffarnus et al. (2011)
#'
#' @format A data frame with 9 rows and 1 variables:
#' \describe{
#'   \item{Condition}{Baseline vs. policy condition}
#'   \item{Time}{Time of study}
#'   \item{SC}{Slope change dummy code}
#'   \item{yhat}{Predicted value from model}
#'   \item{Count1}{Count for site 1}
#'   \item{Count2}{Count for site 2}
#'   \item{Count3}{Count for site 3}
#'   \item{Count4}{Count for site 4}
#'   \item{Facet}{Facet/subplot number}
#' }
#' @source <doi:https://doi.org/10.1002/jaba.967>
"GelinoEtAl2022"

#' @title Plotting data for Hypothetical Academic MTSS
#'
#' @description Plotting data for Hypothetical Academic MTSS
#'
#' @format A data frame with 168 rows and 7 variables:
#' \describe{
#'   \item{Rates}{Rates of change}
#'   \item{Times}{Multiplier for model level}
#'   \item{index}{Individual id}
#'   \item{starts}{Modeled baseline start}
#'   \item{jitter}{Jitter offset}
#'   \item{pred}{Prediction from model}
#'   \item{err}{Residual error}
#' }
"SimulatedAcademicFluency"

#' @title Twitter chart challenge data 1
#'
#' @description Twitter chart challenge data 1
#'
#' @format A data frame with 226 rows and 11 variables:
#' \describe{
#'   \item{StudyID}{Extracted study ID}
#'   \item{FigureNum}{Extracted study figure number}
#'   \item{PanelNum}{Extracted study panel number}
#'   \item{CaseName}{Extracted study case name}
#'   \item{CaseNum}{Extracted study number}
#'   \item{X}{Session}
#'   \item{OutcomeName}{Extracted study outcome name}
#'   \item{Direction}{Direction of trend}
#'   \item{Y}{Outcome measure}
#'   \item{CondName}{Extracted study condition}
#'   \item{CondNum}{Extracted study number}
#' }
"Challenge1Data"

#' @title Twitter chart challenge data 2
#'
#' @description Twitter chart challenge data 2
#'
#' @format A data frame with 113 rows and 5 variables:
#' \describe{
#'   \item{Participant}{Participant name}
#'   \item{Session}{Session number}
#'   \item{Condition}{Condition name}
#'   \item{IWS}{Incorrect word sequences}
#'   \item{CWS}{Correct word sounds}
#' }
"Challenge2Data"

#' @title Twitter chart challenge data 4
#'
#' @description Twitter chart challenge data 4
#'
#' @format A data frame with 189 rows and 11 variables:
#' \describe{
#'   \item{Session}{Session number}
#'   \item{Participant}{Participant name}
#'   \item{Phase}{Phase name}
#'   \item{Number.Writing.Fluency}{Fluency of number writing}
#'   \item{Dot.Number}{Fluency of dot number skills}
#'   \item{Dot.Number.Total}{Fluency of dot number skills on all sets}
#'   \item{Number.Total}{Number writing fluency on all sets}
#'   \item{Number.Writing.Fluency_Accuracy}{Number writing accuracy}
#'   \item{Dot.Number_Accuracy}{Dot number accuracy}
#'   \item{Dot.Number.Total_Accuracy}{Dot number accuracy on all sets}
#'   \item{Number.Total_Accuracy}{Number writing accuracy on all sets}
#' }
"Challenge4Data"
