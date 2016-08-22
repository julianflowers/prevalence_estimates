# prevalence_estimates
What is disease prevalence?

Disease prevalence is a measure of how many cases of any given disease there is in the population at any one time. This will depend on a number of factors:

- The characteristics of the population - genetic make-up, health behaviours, environmental exposure
- The characteristics of the disease - how it develops, whether it is chronic or acute
- The health care response - whether diseases are detected and treated.

It is important to estimate disease prevalence for a number of reasons:

- Population burden of disease - what contributes to the overall health of the population
- Needs assessment
- Planning and commissioning services
- Targeting health improvement
- Devising and developing preventive services
- Undersatnding and explaining use of services
- Understanding spend in relation to disease burden
- Evaluating preventive interventions
- Case finding services

Why estimate?

For many diseases or risk factors, evidence suggests that there are undetected or undiagnosed cases. There are very few comprehensive population registers which aim to capture and count all known cases of a disease - the exception is cancer. Although there are population registers developed in primary care to support the QOF these are based on identifying patients with disease codes in GP systems, rather than actively seeking to identify cases. This means that the size of registers will depend not just on how many people actually have a disease in question, but how likely to they are to seek help from their GP, to have the disease detected,  to have their disease recorded on a GP system, and for that disease to be accurately or appropriately coded so the QOF extraction process can identify those patients.

For this reason it is helpful to have an independent estimate of how much disease might be expected for a given population

What is an estimate?

We therefore estimate how much disease there is. To do this we develop predictive models - statistical models which relate whether or not a person has given disease to known risk factors for that disease. These risk factors are often identified through literature reviews which help us build models which can then be tested and refined.

The aim is find a combination of risk factors which best discriminate between people with and without disease. These models are never 100% accurate.

The other key element is to apply models to populations. For this we need to be able to breakdown the population into subgroups depending on the factors identified in the model. For many populations there are a limited number of factors for which this is possible and this impacts on the risk factors we include in the models.

The data sources used for this work are:

- Surveys (especially Health Survey for England)
- General practice data - especially CPRD
- Hospital data

The methods are set out in detail for each estimate in a series of technical documents but in brief:

- For each disease a case definition is developed based on codes, self report, or prescribing depending on the data sets.
- These data sources are searched to identify these cases, and key risk factor data are extracted
- Often a control group of people who do not have the disease is identified matched for age and sex to the cases
- The relationship between risk factors and disease is modelled and compared with the control group
- This model allows us to calculate the probability of disease given a range of factors for each combination of risk factors or variables.
- These probabilities can be converted to prevalence in a population

Which diseases?

6 conditions are included in this release:
        
        Asthma (due)
        
        Chronic obstructive pulmonary disease
        
        Depression
        
        Cardiovascular disease
                CHD (heart disease)
                Stroke
                Peripheral arterial disease +/- IMD
                
        Hypertension (high blood pressure)
        
        Heart failure (due August)

What does the data show?

The data we have so far is available from this site. We have uploaded the Excel files for LA estimates we have available, and combined GP datasets into a single .csv file for ease of use.

We haven't undertaken in depth analysis but the R script and R notebook outputs of work to date are available from https://beta.rstudioconnect.com/julian_flowers/gp_prev/.
https://beta.rstudioconnect.com/connect/#/apps/1486?dopub=1
https://beta.rstudioconnect.com/connect/#/apps/1359?dopub=1

How were the estimates created?

The technical documents are all available from this site.

How can the data be used?

There are a number of ways the data might be used.
        By General practices

        By commissioners

        By PH teams

        By researchers

        By data scientists

        By policy makers
        
        
Where can I get the data?

The data can be obtained from this site. Or for GP data from this link 
https://beta.rstudioconnect.com/connect/#/apps/1357

Where can I get support?

How do the estimates compare with previous work?

