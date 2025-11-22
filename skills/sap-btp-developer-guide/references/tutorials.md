# SAP BTP Tutorials Reference

## Overview

SAP BTP provides structured learning paths through missions and tutorials, covering CAP and ABAP Cloud development scenarios.

## Account Options

### Free Tier for SAP BTP

**Best for**: Productive projects

- 30+ free-tier services
- Upgrade capability to paid plans
- Available under BTPEA, CPEA, Pay-As-You-Go
- One free plan per runtime (CF/Kyma)

### SAP BTP Trial

**Best for**: Learning and experimentation

- 90-day trial period
- 30+ trial services
- Instant access
- Learning materials included

## CAP Learning Path

### Mission 1: Full-Stack CAP Application (Starter)

**Sample Application**: Incident Management

**Modules:**

| Module | Topics |
|--------|--------|
| Core Development | Environment setup, CAP basics, Fiori Elements UI, custom logic, local launchpad, authorization, testing |
| Cloud Foundry Deployment | CF deployment, SAP Build Work Zone, CI/CD |
| Kyma Deployment | Kyma deployment, Work Zone, CI/CD |

**Discovery Center**: Mission 4033/4432

### Mission 2: Side-by-Side Extension

**Sample Application**: Incident Management + S/4HANA

**Tracks:**

| Track | Runtime | Backend |
|-------|---------|---------|
| Remote Service - CF | Cloud Foundry | Mock Server |
| Remote Service - CF | Cloud Foundry | S/4HANA Cloud |
| Remote Service - Kyma | Kyma | Mock Server |
| Remote Service - Kyma | Kyma | S/4HANA Cloud |
| Eventing - CF | Cloud Foundry | Mock/S/4HANA |
| Eventing - Kyma | Kyma | Mock/S/4HANA |

**Prerequisites**: Complete remote service before eventing

**Discovery Center**: Mission 3999

### Mission 3: Enterprise-Grade CAP Application

**Features:**
- Change tracking
- Audit logging
- Attachment uploads

**Sample Application**: Incident Management (enhanced)

**Discovery Center**: Mission 4364

### Mission 4: Multitenant CAP Application

**Sample Application**: Incident Management (SaaS)

**Entry Point Options:**

| Approach | Consumer Requirements | Site Management |
|----------|----------------------|-----------------|
| Central | SAP Build Work Zone instance | Consumer managed |
| Local | None | Provider managed |

**Key Features:**
- Tenant isolation
- Subscription lifecycle
- Key user extensibility

**Discovery Center**: Mission 4497

### Mission 5: Observability in CAP Application

**Tool**: SAP Cloud Logging

**Capabilities:**
- Logs
- Metrics
- Traces

**Discovery Center**: Mission 4432/4718

## ABAP Learning Path

### RAP100 Basics (Transactional)

**Sample Application**: Flight Reference Scenario

**Tutorials:**

| Tutorial | Topics |
|----------|--------|
| Understanding RAP | Fundamentals, architecture |
| Database Tables | Table creation, data elements |
| UI Service Generation | Automatic CDS/RAP generation |
| Data Model Enhancement | Associations, annotations |
| OData Streams | Large object handling |
| Business Object Behavior | Numbering, determinations, validations |
| Fiori App Deployment | UI deployment, testing |
| Fiori Launchpad | Integration, tiles |

### RAP100 Intermediate

**Tutorials:**

| Tutorial | Topics |
|----------|--------|
| Instance Actions | Custom actions on entities |
| Factory Actions | Create with copy |
| Dynamic Feature Control | Conditional UI elements |
| ABAP Unit Testing | Test class creation |

### RAP120 (AI-Assisted)

**Tool**: SAP Joule

**Capabilities:**
- Predictive code completion
- ABAP Cloud Generator
- Unit test generation

**GitHub**: RAP120 sample project

### Analytical Scenario

**Tutorials:**

| Level | Tutorial |
|-------|----------|
| Beginner | Develop and Consume Queries on SAP Analytics Cloud |
| Intermediate | Queries Based on Booking Supplement |

**Integration**: SAP Analytics Cloud with InA-enabled CDS

### Certifications

| Certification | Focus |
|---------------|-------|
| SAP Certified Associate - Back-End Developer - ABAP Cloud | RAP, CDS, ABAP Cloud |

## Partner Tutorials

### Poetry Slam Manager

**Type**: Reference application for multitenant SaaS

**GitHub**: Available with accompanying tutorials

**Topics:**
- Multitenant architecture
- Partner deployment
- Cost optimization

### Catering Management Extension

**Type**: Customer extension example

**GitHub**: Available with tutorials

**Topics:**
- Tenant-specific extensions
- Key user extensibility

## Learning Resources

### SAP Learning Journeys

| Journey | Focus |
|---------|-------|
| Practicing Clean Core Extensibility for SAP S/4HANA Cloud | Extensions |
| Acquiring Core ABAP Skills | ABAP fundamentals |

### Additional Resources

| Resource | URL |
|----------|-----|
| SAP Developers | https://developers.sap.com/ |
| SAP Learning | https://learning.sap.com/ |
| SAP Community | https://community.sap.com/ |
| SAP Discovery Center | https://discovery-center.cloud.sap/ |

## Prerequisites Management

### Entitlements

Before starting missions:
1. Open BTP Cockpit
2. Navigate to Entitlements
3. Configure quotas for services
4. Assign to subaccount

### Common Services Required

| Service | Plan | Purpose |
|---------|------|---------|
| SAP HANA Cloud | hana | Database |
| Cloud Foundry Runtime | MEMORY | Application runtime |
| SAP Build Work Zone | standard | Launchpad |
| Continuous Integration & Delivery | default | CI/CD |

## Source Documentation

- CAP Tutorials: https://github.com/SAP-docs/btp-developer-guide/blob/main/docs/tutorials-for-sap-cloud-application-programming-model-eb7420a.md
- ABAP Tutorials: https://github.com/SAP-docs/btp-developer-guide/blob/main/docs/tutorials-for-abap-cloud-fd87aaa.md
- Starter Mission: https://github.com/SAP-docs/btp-developer-guide/blob/main/docs/starter-mission-develop-a-full-stack-cap-application-ebd19b5.md
- Extension Mission: https://github.com/SAP-docs/btp-developer-guide/blob/main/docs/mission-develop-a-side-by-side-cap-based-extension-application-2289e25.md
- Multitenant Mission: https://github.com/SAP-docs/btp-developer-guide/blob/main/docs/mission-develop-a-multitenant-cap-application-6d2cbe9.md
- Enterprise Mission: https://github.com/SAP-docs/btp-developer-guide/blob/main/docs/mission-develop-an-enterprise-grade-cap-application-b5be786.md
- Observability Mission: https://github.com/SAP-docs/btp-developer-guide/blob/main/docs/mission-implement-observability-in-a-full-stack-cap-application-c5636db.md
