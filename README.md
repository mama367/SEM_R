# SEM_R

# Structural Equation Modeling (SEM):  
## Gender, Gender Equality Perceptions, and Job Satisfaction

This repository presents an applied example of Structural Equation Modeling (SEM)  
using survey data from the 2023 Korean Journalists Survey.

The project demonstrates how latent constructs, mediation effects, and ideological controls  
can be jointly analyzed within a single SEM framework.

---

## Research Motivation

Survey-based concepts such as gender equality and job satisfaction are not directly observable.  
Analyzing them as single observed variables ignores measurement error and obscures underlying mechanisms.

This project addresses the following questions:

1. Can gender equality perceptions be modeled as a latent construct using multiple survey items?
2. Does gender influence job satisfaction indirectly through gender equality perceptions?
3. How does organizational ideological orientation shape these relationships?

---

## Why Structural Equation Modeling (SEM)?

SEM is particularly suitable for this analysis because it allows researchers to:

- Construct latent variables from multiple observed indicators
- Explicitly account for measurement error
- Estimate direct and indirect (mediation) effects simultaneously
- Integrate measurement models and structural models within a single framework

In this project, SEM combines:
- Confirmatory Factor Analysis (CFA)
- Path analysis
- Mediation modeling

---

## Model Overview

### Latent Variables

**Gender Equality Perceptions (GE)**
- GE_HR
- GE_culture
- GE_inviol
- GE_outviol

**Job Satisfaction (SAT)**
- satis_job
- satis_org
- satis_task

**Organizational Ideology (IDEO)**
- libcon_org (single-indicator latent variable)

---

### Structural Paths

- `female → GE`
- `IDEO → GE`
- `GE → SAT`
- `female → SAT`
- `IDEO → SAT`

This structure allows the estimation of:
- Direct effects of gender and ideology
- Indirect (mediated) effects of gender via GE

---

## Key Findings

- Gender equality perceptions form a statistically valid latent construct
- Gender has no significant direct effect on job satisfaction
- Gender affects job satisfaction indirectly through gender equality perceptions
- Higher sensitivity to gender equality issues is associated with lower job satisfaction
- Organizational ideology independently influences both GE and SAT

These results highlight the importance of modeling latent mechanisms rather than relying on direct regressions.

---

## Methodological Notes

- Estimation method: Maximum Likelihood (ML)
- Sample size: N = 2,011
- Model fit assessed using standard SEM diagnostics
- Indirect effects evaluated within the structural model
- The analysis is theory-driven rather than fit-driven
