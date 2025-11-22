# SAP Cloud Transport Management - Import Operations Reference

**Source**: https://github.com/SAP-docs/btp-cloud-transport-management/tree/main/docs/30-using-import-queue

---

## Import Queue Overview

The import queue is the primary interface for managing transport requests in a transport node.

### Access Methods

1. **Via Home Screen**: Transport Nodes > Select node
2. **Via Landscape Visualization**: Select node > "Go to this node's import queue"

### Queue Interface Components

**Header Section**:
- Transport node name and description
- Forward Mode assignment
- Content Type (if assigned)
- Destination (if assigned)
- Application Type and Region Key (from destination)

**Tabs**:
- **Import Queue**: Transport requests with search/filter/actions
- **Node Details**: General node information and scheduling
- **Transport Routes**: Associated routes

---

## Prerequisites

| Action | Required Role(s) |
|--------|-----------------|
| Import all requests | ImportOperator |
| Import selected requests | TransportOperator OR ImportSelectedOperator |
| Add files | Administrator OR ExportOperator |
| Forward requests | Administrator OR TransportOperator |
| Reset requests | Administrator OR TransportOperator |
| Schedule imports | TransportOperator |
| Enable automatic import | Administrator OR TransportOperator |

---

## Adding Files to Import Queue

**Prerequisites**:
- Administrator or ExportOperator role
- "Allow Upload to Node" enabled on transport node
- Supported content types: MTA, XSC DU, or Application Content
- **Not supported**: BTP ABAP manual uploads

**Constraints**:
- Maximum file size: 1 GB (500 MB on free plan)
- Subscription-level quota applies
- Automatic deletion: 30 days after import completion (unless Initial, Repeatable, or Fatal)

### Procedure

1. Select **Add** in the import queue
2. Complete the dialog:

| Field | Details |
|-------|---------|
| Transport Description | Describes the upload |
| Content Type | Auto-selected if destination exists; otherwise select |
| File | `.mtar` (MTA), `.tgz` (XSC DU), `.zip` (Application Content) |

3. Select **OK**
4. Monitor progress via upload indicator
5. Transport request automatically added to queue

---

## Import Methods

### Import All

Executes all transport requests sequentially as they appear in the queue.

**Note**: Includes requests not visible due to filter settings.

**Eligible Statuses**: Initial, Fatal, Repeatable

**Result**: Succeeded status (or error status if failed)

### Import Selected

Import specific transport requests.

**Availability**: All content types **except** BTP ABAP

**Warning**: May cause inconsistencies if dependent requests aren't imported.

### Import Upto (BTP ABAP Only)

Imports all requests up to a specific transport request.

**Behavior with identical software component references**:
- Only selected request content imports
- Previous requests receive Skipped status

**Error**: Displayed if relevant requests hidden by filters

---

## Forward Transport Requests

Manual forwarding of requests to target nodes.

**Prerequisites**:
- Administrator or TransportOperator role
- Forward Mode set to **Manual** on transport node
- Archive attached to transport request
- Status: Error, Skipped, Succeeded, or Warning

**Selection Order**: Requests forward in selection order, influencing target queue order.

### Status Transitions Upon Forwarding

| Original Status | New Status |
|-----------------|------------|
| Initial | Initial |
| Deleted, Error, Fatal, Skipped, Succeeded, Warning | Repeatable |
| Repeatable | Repeatable |

**Note**: Forwarding requires intact archive; retention periods vary by plan.

---

## Reset Transport Requests

Reset requests to repeat imports.

**Eligible Statuses**: Error, Skipped, or Succeeded

**Result**: Status changes to **Repeatable**

### Procedure

1. Select transport requests in import queue
2. Select **Reset**
3. Requests now have Repeatable status, available for re-import

---

## Schedule Imports

Automate imports at defined intervals.

**Prerequisites**:
- TransportOperator role
- Regular import allowed in queue
- Access from import queue or Landscape Visualization Properties panel

### Import Patterns

**Daily Pattern**:
- Select time interval: "Every hour", "Four times per day", etc.

**Weekly Pattern**:
- Select weekly interval: "Every week", "Every second week", etc.
- Set Execution Time (start time)
- Choose specific days

### Activation

- Schedules are **inactive** by default
- Select **Active** checkbox to enable

### Visual Indicators

- **IMPORT SCHEDULE DETECTED** button in queue header
- Shows job status and next execution time

### Auto-Deactivation Rule

Schedules automatically deactivate after:
- 3 consecutive fatal failures
- Over a period of at least 3 weeks

Affected nodes appear in **Import Schedules** section on home screen.

---

## Enable Automatic Import

Immediate processing of all importable requests when new requests arrive.

**Prerequisites**:
- Administrator or TransportOperator role
- Importing not disabled in node

**Warning**: Activating immediately processes all requests with Initial, Fatal, or Repeatable statuses.

### Effects When Enabled

**Visual**:
- "AUTOMATIC IMPORT ENABLED" label in node header

**Functional**:
- Existing importable requests processed immediately
- New requests trigger automatic background imports (Import All)
- Failed imports retried when new requests arrive
- Manual importing removed from queue
- Enable button converts to disable

**System**:
- Existing import schedules become inactive
- Action logged as "Automatic Import Enabled" = true
- Landscape Visualization shows "Import Automatically" = Yes

---

## Transport Request Statuses

### Import Statuses (in Import Queues)

| Status | Description | Importable |
|--------|-------------|------------|
| Initial | Added but not imported | Yes |
| Running | Import in progress | No |
| Succeeded | Import successful | No |
| Warning | Completed with warnings | No |
| Error | Failed due to error | No |
| Fatal | Failed due to fatal error | Yes |
| Skipped | Intentionally skipped | No |
| Repeatable | Reset for re-import | Yes |
| Deleted | Removed from queue | No |
| Transient | Tested modifiable request, then released | No |

### Lifecycle Statuses (in Transport Requests Overview)

| Status | Description |
|--------|-------------|
| Modifiable | Request can be edited |
| Released | In at least one queue with non-archived/deleted status |
| Deleted | Deleted from all queues |
| Archived | Cleaned up by retention policy |

---

## Modifiable Transport Requests

Allow efficient management of multiple files in a single request.

### Create Modifiable Request

**Prerequisites**:
- Access to transport requests overview
- ExportOperator role for source node

**Procedure**:

1. Select **Create** from overview
2. Complete dialog:
   - **Source Node**: Choose from nodes with uploads enabled
   - **Upload in Source Node**: If checked, imports to source; if unchecked, exports to follow-on nodes
   - **Content Type**: Must match source node
   - **Description**: Request details
3. Select **Create** (creates empty request)

### Add Files

1. Click transport request row
2. Navigate to **Content** tab
3. Select **Add**
4. Browse and select file (must match content type)
5. Choose **Upload**

**Capabilities**:
- Multiple files without limit
- Remove and rearrange before testing
- Request invisible in import queues until Test mode

---

## Storage Capacity

### Limits by Plan

| Plan | Total Capacity | File Limit | Retention |
|------|----------------|------------|-----------|
| Standard | 50 GB | 1 GB | 1-30 days (default 30) |
| Free | 500 MB | 500 MB | 1-7 days (default 7) |

### Partner-Managed Edition

| Plan | Total Capacity | File Limit | Retention |
|------|----------------|------------|-----------|
| Standard | 10 GB | 400 MB | 1-30 days |
| Free | 500 MB | 400 MB | 1-7 days |

### Automatic Cleanup

Files deleted when:
- Requests reach final status (Deleted, Error, Skipped, Succeeded, Warning)
- Retention period elapsed since last action
- Exception: Deleted status requests cleaned immediately

Files **NOT** deleted if status is: Fatal, Initial, Repeatable, Running

### Capacity Alerts

- Warning at 85% usage
- No uploads when maximum reached
- Manual deletion frees space

---

## Documentation Links

- Import Queue: https://github.com/SAP-docs/btp-cloud-transport-management/blob/main/docs/30-using-import-queue/using-the-import-queue-3c4b6f3.md
- Import Requests: https://github.com/SAP-docs/btp-cloud-transport-management/blob/main/docs/30-using-import-queue/import-transport-requests-d2005d5.md
- Add Files: https://github.com/SAP-docs/btp-cloud-transport-management/blob/main/docs/30-using-import-queue/add-files-to-import-queues-c3c87cb.md
- Schedule Imports: https://github.com/SAP-docs/btp-cloud-transport-management/blob/main/docs/30-using-import-queue/schedule-imports-110a7a4.md
- Automatic Import: https://github.com/SAP-docs/btp-cloud-transport-management/blob/main/docs/30-using-import-queue/enable-automatic-import-9171d39.md
- Request Statuses: https://github.com/SAP-docs/btp-cloud-transport-management/blob/main/docs/40-using-request-overview/statuses-of-transport-requests-3a8259e.md
