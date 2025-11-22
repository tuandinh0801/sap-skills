# SAP Analytics Cloud - Analytics Designer API Reference for Planning

**Source**: https://help.sap.com/doc/958d4c11261f42e992e8d01a4c0dde25/release/en-US/index.html
**Version**: 2025.14+
**Last Updated**: 2025-11-22

---

## Table of Contents

1. [Application Class](#application-class)
2. [DataSource API](#datasource-api)
3. [Planning API (getPlanning)](#planning-api)
4. [PlanningModel API](#planningmodel-api)
5. [Data Actions API](#data-actions-api)
6. [Multi Actions API](#multi-actions-api)
7. [Widget APIs](#widget-apis)
8. [Utility Classes](#utility-classes)
9. [Enumerations](#enumerations)

---

## Application Class

The Application object provides access to global application functionality.

### Properties & Methods

| Method | Description | Return Type |
|--------|-------------|-------------|
| `getInfo()` | Get application information | ApplicationInfo |
| `getMode()` | Get current mode (View, Present, Embed) | ApplicationMode |
| `getTheme()` | Get current theme | Theme |
| `getWidgets()` | Get all widgets in application | Widget[] |
| `getUserInfo()` | Get current user information | UserInfo |
| `showBusyIndicator()` | Show loading indicator | void |
| `hideBusyIndicator()` | Hide loading indicator | void |
| `refreshData()` | Refresh all data sources | void |
| `showMessage(text)` | Show toast message | void |
| `sendNotification(text)` | Send notification to user | void |
| `postMessage(message)` | Post message for embedding scenarios | void |

### Event Handlers

| Event | Trigger Condition |
|-------|-------------------|
| `onInitialization` | Application loads |
| `onResize` | Browser window resizes |
| `onOrientationChange` | Device orientation changes |
| `onTimeout` | Session timeout warning |
| `onBeforeExecute` | Before data action executes |

### Example: Application Initialization

```javascript
// onInitialization event
Application.showBusyIndicator();

// Initialize default filters
var currentYear = new Date().getFullYear().toString();
Table_1.getDataSource().setDimensionFilter("Year", currentYear);

// Load user preferences
var userInfo = Application.getUserInfo();
console.log("User: " + userInfo.userId);

Application.hideBusyIndicator();
```

---

## DataSource API

Access data source functionality for querying and filtering.

### Core Methods

| Method | Description | Parameters |
|--------|-------------|------------|
| `getDimensions()` | Get all dimensions | none |
| `getMeasures()` | Get all measures | none |
| `getMembers(dimId, options)` | Get dimension members | dimId: string, options: MemberOptions |
| `getMember(dimId, memberId)` | Get single member | dimId: string, memberId: string |
| `setDimensionFilter(dimId, filter)` | Set filter on dimension | dimId: string, filter: string/MemberInfo[] |
| `removeDimensionFilter(dimId)` | Remove dimension filter | dimId: string |
| `getDimensionFilters(dimId)` | Get active filters | dimId: string |
| `setVariableValue(varId, value)` | Set variable value | varId: string, value: string |
| `getVariables()` | Get all variables | none |
| `setHierarchy(dimId, hierarchyId)` | Set dimension hierarchy | dimId: string, hierarchyId: string |
| `getHierarchyLevel(dimId)` | Get current hierarchy level | dimId: string |

### MemberOptions Object

```javascript
{
    limit: number,           // Max members to return
    offset: number,          // Skip first N members
    accessMode: MemberAccessMode,  // BookedValues, AllValues
    search: string,          // Search string
    hierarchyId: string      // Specific hierarchy
}
```

### Filter Syntax (MDX)

```javascript
// Single member
"[Version].[parentId].&[public.Actual]"

// Multiple members using array
var members = ["2024", "2025"];
Table_1.getDataSource().setDimensionFilter("Year", members);

// Using MemberInfo objects
var memberInfos = [
    {id: "2024", description: "Year 2024"},
    {id: "2025", description: "Year 2025"}
];
Table_1.getDataSource().setDimensionFilter("Year", memberInfos);
```

### Example: Dynamic Filtering

```javascript
// Get booked values only (performance optimization)
var accounts = Table_1.getDataSource().getMembers("Account", {
    accessMode: MemberAccessMode.BookedValues,
    limit: 1000
});

// Filter to specific accounts
var selectedAccounts = [];
for (var i = 0; i < accounts.length; i++) {
    if (accounts[i].properties.AccountType === "Revenue") {
        selectedAccounts.push(accounts[i].id);
    }
}

Table_1.getDataSource().setDimensionFilter("Account", selectedAccounts);
```

---

## Planning API

Access planning functionality on tables via `getPlanning()`.

### Methods

| Method | Description | Return Type |
|--------|-------------|-------------|
| `isEnabled()` | Check if planning enabled | boolean |
| `setEnabled(enabled)` | Enable/disable planning | void |
| `getPublicVersions()` | Get all public versions | PlanningVersion[] |
| `getPublicVersion(versionId)` | Get specific public version | PlanningVersion |
| `getPrivateVersion()` | Get current private version | PlanningVersion |
| `getPrivateVersions()` | Get all user's private versions | PlanningVersion[] |
| `setUserInput(selection, value)` | Set cell value | void |
| `submitData()` | Submit pending changes | void |
| `getDataLocking()` | Get data locking object | DataLocking |

### PlanningVersion Object

| Property/Method | Description |
|----------------|-------------|
| `id` | Version identifier |
| `description` | Version description |
| `isDirty()` | Check for unsaved changes |
| `publish()` | Publish version changes |
| `publishAs(newId, newDesc)` | Publish as new version |
| `revert()` | Revert unpublished changes |

### DataLocking Object

| Method | Description |
|--------|-------------|
| `getState(selection)` | Get lock state for cells |
| `setState(selection, state)` | Set lock state (owner only) |
| `getOwners(selection)` | Get lock owners |

### Example: Version Management

```javascript
// Get all public versions
var versions = Table_1.getPlanning().getPublicVersions();

for (var i = 0; i < versions.length; i++) {
    console.log(versions[i].id + ": " + versions[i].description);
}

// Work with private version
var privateVer = Table_1.getPlanning().getPrivateVersion();
if (privateVer && privateVer.isDirty()) {
    // Confirm before publishing
    privateVer.publish();
    Application.showMessage("Changes published successfully");
}
```

### Example: Data Entry

```javascript
// Get current selection
var selection = Table_1.getSelections()[0];

// Check if locked
var dataLocking = Table_1.getPlanning().getDataLocking();
var lockState = dataLocking.getState(selection);

if (lockState !== DataLockingState.Locked) {
    // Set value (use "*" prefix for factor multiplication)
    Table_1.getPlanning().setUserInput(selection, "1000");

    // Or multiply existing value by 1.1
    Table_1.getPlanning().setUserInput(selection, "*1.1");

    // Submit changes
    Table_1.getPlanning().submitData();
}
```

---

## PlanningModel API

Access planning model for master data operations.

### Methods

| Method | Description | Parameters |
|--------|-------------|------------|
| `getMembers(dimId, options)` | Get dimension members | dimId: string, options: object |
| `getMember(dimId, memberId)` | Get single member | dimId: string, memberId: string |
| `createMembers(dimId, members)` | Create new members | dimId: string, members: MemberData[] |
| `updateMembers(dimId, members)` | Update existing members | dimId: string, members: MemberData[] |
| `deleteMembers(dimId, memberIds)` | Delete members | dimId: string, memberIds: string[] |

### MemberData Object

```javascript
{
    id: string,              // Member ID (required)
    description: string,     // Display text
    parentId: string,        // Parent in hierarchy
    properties: {            // Custom properties
        "PROPERTY_NAME": "value"
    }
}
```

### Example: Master Data Management

```javascript
// Get all cost centers with properties
var costCenters = PlanningModel_1.getMembers("CostCenter", {
    limit: 5000
});

// Find members by property
var activeCCs = [];
for (var i = 0; i < costCenters.length; i++) {
    if (costCenters[i].properties.Status === "Active") {
        activeCCs.push(costCenters[i]);
    }
}

// Create new cost center
PlanningModel_1.createMembers("CostCenter", [{
    id: "CC_NEW_001",
    description: "New Cost Center",
    parentId: "CC_PARENT",
    properties: {
        "Status": "Active",
        "Region": "EMEA"
    }
}]);

// Update existing
PlanningModel_1.updateMembers("CostCenter", [{
    id: "CC_NEW_001",
    description: "Updated Cost Center Name"
}]);
```

### Important Limitations

- `getMember()` not officially supported for Version dimension
- Always check model type before master data operations
- Operations require appropriate permissions

---

## Data Actions API

Execute data actions programmatically.

### Methods

| Method | Description |
|--------|-------------|
| `execute()` | Execute synchronously |
| `executeInBackground()` | Execute asynchronously |
| `setParameterValue(paramId, value)` | Set parameter value |
| `getParameterValue(paramId)` | Get parameter value |

### Events

| Event | Description |
|-------|-------------|
| `onExecutionStatusUpdate` | Status changed during execution |
| `onExecutionComplete` | Execution finished |

### Example: Data Action Execution

```javascript
// Set parameters
DataAction_Copy.setParameterValue("SourceVersion", "Actual");
DataAction_Copy.setParameterValue("TargetVersion", "Budget");
DataAction_Copy.setParameterValue("Year", "2025");

// Execute synchronously
DataAction_Copy.execute();

// Or execute in background
DataAction_Copy.executeInBackground();

// Handle completion (in onExecutionComplete event)
Application.showMessage("Data action completed successfully");
Application.refreshData();
```

---

## Multi Actions API

Execute multi actions for orchestrated operations.

### Methods

| Method | Description |
|--------|-------------|
| `execute()` | Execute multi action |
| `executeInBackground()` | Execute asynchronously |
| `setParameterValue(paramId, value)` | Set cross-step parameter |
| `getParameterValue(paramId)` | Get parameter value |

### Example: Multi Action Execution

```javascript
// Set cross-model parameters
MultiAction_Planning.setParameterValue("PlanningCycle", "2025Q1");
MultiAction_Planning.setParameterValue("Region", "EMEA");

// Execute planning workflow
MultiAction_Planning.execute();

// Typical multi action flow:
// 1. Clean target version
// 2. Copy actuals
// 3. Run forecast
// 4. Apply allocations
// 5. Publish results
// 6. Lock data
```

---

## Widget APIs

### Table Widget

| Method | Description |
|--------|-------------|
| `getPlanning()` | Get planning object |
| `getDataSource()` | Get data source |
| `getSelections()` | Get selected cells |
| `addDimensionToRows(dimId)` | Add dimension to rows |
| `addDimensionToColumns(dimId)` | Add dimension to columns |
| `removeDimension(dimId)` | Remove dimension |
| `setVisible(visible)` | Show/hide table |
| `rankBy(measure, order, count)` | Apply ranking |
| `sortByValue(measure, order)` | Sort by measure |

### Chart Widget

| Method | Description |
|--------|-------------|
| `getDataSource()` | Get data source |
| `addDimension(dimId)` | Add dimension |
| `addMeasure(measureId)` | Add measure |
| `removeDimension(dimId)` | Remove dimension |
| `setVisible(visible)` | Show/hide chart |
| `getDataChangeInsights()` | Get variance insights |
| `getForecast()` | Get forecast data |

### Input Controls (Dropdown, ListBox, etc.)

| Method | Description |
|--------|-------------|
| `getSelectedKey()` | Get selected value |
| `setSelectedKey(key)` | Set selected value |
| `getSelectedKeys()` | Get multiple selections |
| `setSelectedKeys(keys)` | Set multiple selections |
| `addItem(key, text)` | Add item |
| `removeItem(key)` | Remove item |
| `removeAllItems()` | Clear all items |

---

## Utility Classes

### ConvertUtils

| Method | Description |
|--------|-------------|
| `stringToInteger(str)` | Convert string to integer |
| `stringToNumber(str)` | Convert string to float |
| `numberToString(num)` | Convert number to string |
| `dateToString(date, format)` | Format date |

### ArrayUtils

| Method | Description |
|--------|-------------|
| `create(type, size)` | Create typed array |
| `contains(array, item)` | Check if contains |
| `indexOf(array, item)` | Find index |

### NavigationUtils

| Method | Description |
|--------|-------------|
| `openStory(storyId)` | Open another story |
| `openApplication(appId)` | Open another application |
| `openUrl(url)` | Open external URL |

### DateFormat / NumberFormat

```javascript
// Format date
var formatted = DateFormat.format(new Date(), "yyyy-MM-dd");

// Format number
var formatted = NumberFormat.format(1234567.89, "#,##0.00");
```

---

## Enumerations

### ApplicationMode

| Value | Description |
|-------|-------------|
| `View` | Normal view mode |
| `Present` | Presentation mode |
| `Embed` | Embedded in other application |

### MemberAccessMode

| Value | Description |
|-------|-------------|
| `BookedValues` | Only members with data |
| `AllValues` | All members |

### DataLockingState

| Value | Description |
|-------|-------------|
| `Open` | Data can be edited |
| `Restricted` | Only owner can edit |
| `Locked` | No edits allowed |

### SortOrder

| Value | Description |
|-------|-------------|
| `Ascending` | A to Z, 0 to 9 |
| `Descending` | Z to A, 9 to 0 |
| `Default` | Model default order |

### RankOrder

| Value | Description |
|-------|-------------|
| `Top` | Highest values first |
| `Bottom` | Lowest values first |

---

## Quick Reference Cheat Sheet

```javascript
// === APPLICATION ===
Application.showBusyIndicator();
Application.hideBusyIndicator();
Application.showMessage("text");
Application.getUserInfo();

// === DATA SOURCE ===
Table_1.getDataSource().getDimensions();
Table_1.getDataSource().getMembers("Dim", {limit: 1000});
Table_1.getDataSource().setDimensionFilter("Dim", "value");
Table_1.getDataSource().removeDimensionFilter("Dim");

// === PLANNING ===
Table_1.getPlanning().isEnabled();
Table_1.getPlanning().getPublicVersions();
Table_1.getPlanning().getPrivateVersion();
Table_1.getPlanning().setUserInput(selection, "value");
Table_1.getPlanning().submitData();

// === PLANNING MODEL ===
PlanningModel_1.getMembers("Dim");
PlanningModel_1.createMembers("Dim", [{id: "X", description: "Y"}]);
PlanningModel_1.updateMembers("Dim", [{id: "X", description: "Z"}]);
PlanningModel_1.deleteMembers("Dim", ["X"]);

// === DATA LOCKING ===
Table_1.getPlanning().getDataLocking().getState(selection);

// === DATA ACTIONS ===
DataAction_1.setParameterValue("Param", "Value");
DataAction_1.execute();
DataAction_1.executeInBackground();

// === CONVERSIONS ===
ConvertUtils.stringToInteger("123");
ConvertUtils.numberToString(123);
```

---

**Documentation Links**:
- Full API Reference: https://help.sap.com/doc/958d4c11261f42e992e8d01a4c0dde25/release/en-US/index.html
- SAP Community: https://community.sap.com/topics/sap-analytics-cloud
