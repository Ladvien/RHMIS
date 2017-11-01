# RHMIS
An R package for manipulating [Homeless Management Information System (HMIS)](https://www.hudexchange.info/programs/hmis/) data as defined by Housing and Urban Development (HUD).

## Purpose
These tools are meant to augment any HMIS implementations by creating methods to assist with extracting useful information from an HMIS. The standardization of the HMIS CSVs is leveraged, building ad hoc data warehouses, which can be queried _quickly_ with the pre-written procedures.

This allows any HMIS implementation to be more loosely coupled to their software provider, since all HMIS software providers are required to produce HMIS CSVs.

## HMIS Levels
HMIS data and methods are structured on three levels: System, Agency, and Project.

### System
HUD requires HMIS software vendors implement tools to pull the following standardized reports:

* Annual Homelessness Assessment Report (AHAR)
* Housing Inventory Count (HIC)
* Point-in-Time Count (PIT)
* Annual Performance Report (APR)
* Consolidated Annual Evaluation and Performance Report (CAPER)
* System Performance Report (Sys PM)
* HUD Data Quality Report (HUD DQ)

These reports are helpful to HUD for evaluating the effectiveness of a Continuum of Care (HUD defined collections of homeless service agencies, divided by regions).  However, these standard reports are not always helpful for COC's in meeting local reporting requirements. Local reporting requirements are often critical for COC's as they often provide proof of need, and therefore, allow generation of match funds for COC's to obtain HUD funds.

One of the goals of this toolset is to provide COC's with an additional set of tools which should allow the HMIS Lead to provide non-standard system level reports to local municipalities.
