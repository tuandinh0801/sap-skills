# SAP ABAP Development Skill

Comprehensive ABAP development skill for SAP systems covering classic ABAP and modern ABAP Cloud development patterns.

## Skill Overview

This skill provides extensive knowledge for ABAP development including:

- **Internal Tables**: Standard, sorted, hashed tables; keys; operations; LOOP, READ, MODIFY
- **ABAP SQL**: SELECT, INSERT, UPDATE, DELETE, JOINs, CTEs, aggregate functions
- **Object-Oriented ABAP**: Classes, interfaces, inheritance, polymorphism, events
- **Constructor Expressions**: VALUE, NEW, CONV, CORRESPONDING, COND, SWITCH, REDUCE, FILTER
- **Dynamic Programming**: Field symbols, data references, RTTI, RTTC
- **String Processing**: String functions, templates, FIND, REPLACE, regex
- **RAP (RESTful Application Programming Model)**: EML statements, BDEF, handler methods
- **CDS View Entities**: Annotations, associations, expressions
- **ABAP Unit Testing**: Test classes, assertions, test doubles
- **Exception Handling**: TRY-CATCH, exception classes, messages
- **ABAP Cloud Development**: Released APIs, restrictions, migration patterns

## Auto-Trigger Keywords

This skill activates when discussing:

### ABAP Language
- ABAP, ABAP code, ABAP program, ABAP class, ABAP method
- DATA, TYPES, CONSTANTS, FIELD-SYMBOLS
- IF, CASE, LOOP, DO, WHILE, ENDLOOP, ENDIF
- SELECT, INSERT, UPDATE, DELETE, MODIFY
- TRY, CATCH, RAISE EXCEPTION, CLEANUP
- CLASS, INTERFACE, METHOD, ENDCLASS

### Internal Tables
- internal table, itab, TABLE OF, STANDARD TABLE, SORTED TABLE, HASHED TABLE
- APPEND, INSERT, READ TABLE, MODIFY TABLE, DELETE
- LOOP AT, FIELD-SYMBOL, ASSIGNING, INTO
- table key, secondary key, WITH KEY
- FOR, REDUCE, FILTER

### Constructor Expressions
- VALUE, NEW, CONV, CORRESPONDING, CAST, REF
- COND, SWITCH, EXACT
- REDUCE, FILTER, FOR
- constructor expression, inline declaration

### Object Orientation
- ABAP OO, class definition, class implementation
- inheritance, INHERITING FROM, REDEFINITION
- interface, INTERFACES, ALIASES
- CREATE OBJECT, instantiation, factory method
- PUBLIC SECTION, PRIVATE SECTION, PROTECTED SECTION
- event, RAISE EVENT, SET HANDLER

### RAP and Modern ABAP
- RAP, RESTful Application Programming Model
- EML, Entity Manipulation Language
- MODIFY ENTITIES, READ ENTITIES, COMMIT ENTITIES
- BDEF, behavior definition, handler method, saver method
- managed, unmanaged, draft
- %cid, %control, %tky, mapped, failed, reported

### CDS Views
- CDS, Core Data Services, CDS view entity
- define view entity, association, composition
- annotation, @UI, @Semantics
- input parameter, $session

### ABAP SQL
- ABAP SQL, SELECT, FROM, WHERE, INTO TABLE
- INNER JOIN, LEFT OUTER JOIN, RIGHT OUTER JOIN
- GROUP BY, HAVING, ORDER BY
- aggregate function, COUNT, SUM, AVG, MIN, MAX
- FOR ALL ENTRIES, subquery, CTE

### Dynamic Programming
- field symbol, ASSIGN, UNASSIGN, IS ASSIGNED
- data reference, REF TO, CREATE DATA, dereference
- RTTI, RTTC, cl_abap_typedescr, cl_abap_structdescr
- dynamic SQL, dynamic method call

### String Processing
- string, string template, string function
- FIND, REPLACE, CONCATENATE, SPLIT
- to_upper, to_lower, strlen, substring
- PCRE, regular expression, regex, pattern matching

### Testing
- ABAP Unit, test class, FOR TESTING
- cl_abap_unit_assert, assert_equals
- test double, mock, stub, injection
- RISK LEVEL, DURATION

### Exception Handling
- exception, TRY, CATCH, ENDTRY
- RAISE EXCEPTION, THROW
- cx_root, cx_static_check, cx_dynamic_check
- exception class, get_text

### ABAP Cloud
- ABAP Cloud, ABAP for Cloud Development
- released API, XCO library
- SAP BTP ABAP Environment
- cloud-ready, upgrade-stable

### Errors and Debugging
- sy-subrc, sy-tabix, sy-index
- runtime error, dump, exception
- CX_SY_ZERODIVIDE, CX_SY_ITAB_LINE_NOT_FOUND
- debugging, breakpoint

## Directory Structure

```
sap-abap/
├── SKILL.md                    # Main skill file with quick reference
├── README.md                   # This file (keywords for discoverability)
├── PROGRESS_TRACKING.md        # Content extraction progress
└── references/                 # Detailed reference files
    ├── internal-tables.md      # Complete table operations
    ├── abap-sql.md             # ABAP SQL comprehensive guide
    ├── object-orientation.md   # OO programming patterns
    ├── constructor-expressions.md # Constructor operators
    ├── dynamic-programming.md  # RTTI, RTTC, field symbols
    ├── string-processing.md    # String functions and regex
    ├── rap-eml.md              # RAP and EML reference
    ├── cds-views.md            # CDS view entities
    ├── unit-testing.md         # ABAP Unit framework
    ├── exceptions.md           # Exception handling
    └── cloud-development.md    # ABAP Cloud specifics
```

## Usage

Ask Claude about any ABAP development topic:

- "How do I create a sorted internal table with multiple keys?"
- "What's the syntax for EML CREATE operations in RAP?"
- "Show me how to use CORRESPONDING with field mapping"
- "How do I handle exceptions in ABAP?"
- "What's the difference between ABAP Cloud and classic ABAP?"

## Source Documentation

Content based on official SAP ABAP Cheat Sheets:
- **Repository**: https://github.com/SAP-samples/abap-cheat-sheets
- **SAP Help**: https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm

## Version

- **Skill Version**: 1.0.0
- **Last Updated**: 2025-11-22
- **ABAP Release**: Latest (7.5x / Cloud)
