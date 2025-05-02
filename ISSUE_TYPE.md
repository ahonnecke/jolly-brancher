# Document Title

Issue types

This resource represents issues types. Use it to:

    get, create, update, and delete issue types.
    get all issue types for a user.
    get alternative issue types.
    set an avatar for an issue type.

Get all issue types for user

GET /rest/api/2/issuetype

Returns all issue types.

This operation can be accessed anonymously.

Permissions required: Issue types are only returned as follows:

    if the user has the Administer Jira global permission

, all issue types are returned.
if the user has the Browse projects project permission

    for one or more projects, the issue types associated with the projects the user has permission to browse are returned.
    if the user is anonymous then they will be able to access projects with the Browse projects for anonymous users
    if the user authentication is incorrect they will fall back to anonymous

Connect app scope required: READ
OAuth 2.0 scopes required:
ClassicRECOMMENDED:read:jira-work
Granular:read:issue-type:jira, read:avatar:jira, read:project-category:jira, read:project:jira
Request
There are no parameters for this request.
Example
Forge
cURL
Node.js
Java
Python
PHP
// This sample uses Atlassian Forge
// https://developer.atlassian.com/platform/forge/
import api, { route } from "@forge/api";

const response = await api.asUser().requestJira(route`/rest/api/2/issuetype`, {
  headers: {
    'Accept': 'application/json'
  }
});

console.log(`Response: ${response.status} ${response.statusText}`);
console.log(await response.json());
Responses
200

Returned if the request is successful.
Content type	Value
application/json	

Array<IssueTypeDetails>

Example response (application/json)
[
  {
    "avatarId": 1,
    "description": "A task that needs to be done.",
    "hierarchyLevel": 0,
    "iconUrl": "https://your-domain.atlassian.net/secure/viewavatar?size=xsmall&avatarId=10299&avatarType=issuetype\",",
    "id": "3",
    "name": "Task",
    "self": "https://your-domain.atlassian.net/rest/api/2/issueType/3",
    "subtask": false
  },
  {
    "avatarId": 10002,
    "description": "A problem with the software.",
    "entityId": "9d7dd6f7-e8b6-4247-954b-7b2c9b2a5ba2",
    "hierarchyLevel": 0,
    "iconUrl": "https://your-domain.atlassian.net/secure/viewavatar?size=xsmall&avatarId=10316&avatarType=issuetype\",",
    "id": "1",
    "name": "Bug",
    "scope": {
      "project": {
        "id": "10000"
      },
      "type": "PROJECT"
    },
    "self": "https://your-domain.atlassian.net/rest/api/2/issueType/1",
    "subtask": false
  }
]
Create issue type

POST /rest/api/2/issuetype

Creates an issue type and adds it to the default issue type scheme.

Permissions required: Administer Jira global permission

.

Connect app scope required: ADMIN
OAuth 2.0 scopes required:
ClassicRECOMMENDED:manage:jira-configuration
Granular:write:issue-type:jira, read:avatar:jira, read:issue-type:jira, read:project-category:jira, read:project:jira
Request
Body parameters
description

string

The description of the issue type.
hierarchyLevel

integer

The hierarchy level of the issue type. Use:

    -1 for Subtask.
    0 for Base.

Defaults to 0.
Format: int32
name Required

string

The unique name for the issue type. The maximum length is 60 characters.
type

string

Deprecated. Use hierarchyLevel instead. See the deprecation notice

for details.

Whether the issue type is subtype or standard. Defaults to standard.

Valid values: subtask, standard
Example
Forge
cURL
Node.js
Java
Python
PHP
// This sample uses Atlassian Forge
// https://developer.atlassian.com/platform/forge/
import api, { route } from "@forge/api";

var bodyData = `{
  "description": "description",
  "name": "name",
  "type": "standard"
}`;

const response = await api.asUser().requestJira(route`/rest/api/2/issuetype`, {
  method: 'POST',
  headers: {
    'Accept': 'application/json',
    'Content-Type': 'application/json'
  },
  body: bodyData
});

console.log(`Response: ${response.status} ${response.statusText}`);
console.log(await response.json());
Responses
201
400
401
403
409

Returned if the request is successful.
Content type	Value
application/json	

IssueTypeDetails
Get issue types for project
Experimental

GET /rest/api/2/issuetype/project

Returns issue types for a project.

This operation can be accessed anonymously.

Permissions required: Browse projects project permission
in the relevant project or Administer Jira global permission

.

Connect app scope required: READ
OAuth 2.0 scopes required:
ClassicRECOMMENDED:read:jira-work
Granular:read:issue-type:jira, read:avatar:jira, read:project-category:jira, read:project:jira
Request
Query parameters
projectId Required

integer

The ID of the project.
Format: int64
level

integer

The level of the issue type to filter by. Use:

    -1 for Subtask.
    0 for Base.
    1 for Epic.

Format: int32
Example
Forge
cURL
Node.js
Java
Python
PHP
// This sample uses Atlassian Forge
// https://developer.atlassian.com/platform/forge/
import api, { route } from "@forge/api";

const response = await api.asUser().requestJira(route`/rest/api/2/issuetype/project?projectId={projectId}`, {
  headers: {
    'Accept': 'application/json'
  }
});

console.log(`Response: ${response.status} ${response.statusText}`);
console.log(await response.json());
Responses
200
400
404

Returned if the request is successful.
Content type	Value
application/json	

Array<IssueTypeDetails>

Example response (application/json)
[
  {
    "avatarId": 10002,
    "description": "A problem with the software.",
    "entityId": "9d7dd6f7-e8b6-4247-954b-7b2c9b2a5ba2",
    "hierarchyLevel": 0,
    "iconUrl": "https://your-domain.atlassian.net/secure/viewavatar?size=xsmall&avatarId=10316&avatarType=issuetype\",",
    "id": "1",
    "name": "Bug",
    "scope": {
      "project": {
        "id": "10000"
      },
      "type": "PROJECT"
    },
    "self": "https://your-domain.atlassian.net/rest/api/2/issueType/1",
    "subtask": false
  },
  {
    "avatarId": 1,
    "description": "A task that needs to be done.",
    "hierarchyLevel": 0,
    "iconUrl": "https://your-domain.atlassian.net/secure/viewavatar?size=xsmall&avatarId=10299&avatarType=issuetype\",",
    "id": "3",
    "name": "Task",
    "scope": {
      "project": {
        "id": "10000"
      },
      "type": "PROJECT"
    },
    "self": "https://your-domain.atlassian.net/rest/api/2/issueType/3",
    "subtask": false
  }
]
Get issue type

GET /rest/api/2/issuetype/{id}

Returns an issue type.

This operation can be accessed anonymously.

Permissions required: Browse projects project permission
in a project the issue type is associated with or Administer Jira global permission

.

Connect app scope required: READ
OAuth 2.0 scopes required:
ClassicRECOMMENDED:read:jira-work
Granular:read:issue-type:jira, read:avatar:jira, read:project-category:jira, read:project:jira
Request
Path parameters
id Required

string

The ID of the issue type.
Example
Forge
cURL
Node.js
Java
Python
PHP
// This sample uses Atlassian Forge
// https://developer.atlassian.com/platform/forge/
import api, { route } from "@forge/api";

const response = await api.asUser().requestJira(route`/rest/api/2/issuetype/{id}`, {
  headers: {
    'Accept': 'application/json'
  }
});

console.log(`Response: ${response.status} ${response.statusText}`);
console.log(await response.json());
Responses
200
400
404

Returned if the request is successful.
Content type	Value
application/json	

IssueTypeDetails

Example response (application/json)
{
  "avatarId": 1,
  "description": "A task that needs to be done.",
  "hierarchyLevel": 0,
  "iconUrl": "https://your-domain.atlassian.net/secure/viewavatar?size=xsmall&avatarId=10299&avatarType=issuetype\",",
  "id": "3",
  "name": "Task",
  "self": "https://your-domain.atlassian.net/rest/api/2/issueType/3",
  "subtask": false
}
Update issue type

PUT /rest/api/2/issuetype/{id}

Updates the issue type.

Permissions required: Administer Jira global permission

.

Connect app scope required: ADMIN
OAuth 2.0 scopes required:
ClassicRECOMMENDED:manage:jira-configuration
Granular:write:issue-type:jira, read:avatar:jira, read:issue-type:jira, read:project-category:jira, read:project:jira
Request
Path parameters
id Required

string

The ID of the issue type.
Body parameters
avatarId

integer

The ID of an issue type avatar.
Format: int64
description

string

The description of the issue type.
name

string

The unique name for the issue type. The maximum length is 60 characters.
Example
Forge
cURL
Node.js
Java
Python
PHP
// This sample uses Atlassian Forge
// https://developer.atlassian.com/platform/forge/
import api, { route } from "@forge/api";

var bodyData = `{
  "avatarId": 1,
  "description": "description",
  "name": "name"
}`;

const response = await api.asUser().requestJira(route`/rest/api/2/issuetype/{id}`, {
  method: 'PUT',
  headers: {
    'Accept': 'application/json',
    'Content-Type': 'application/json'
  },
  body: bodyData
});

console.log(`Response: ${response.status} ${response.statusText}`);
console.log(await response.json());
Responses
200
400
401
403
404
409

Returned if the request is successful.
Content type	Value
application/json	

IssueTypeDetails
Delete issue type

DELETE /rest/api/2/issuetype/{id}

Deletes the issue type. If the issue type is in use, all uses are updated with the alternative issue type (alternativeIssueTypeId). A list of alternative issue types are obtained from the Get alternative issue types resource.

Permissions required: Administer Jira global permission

.

Connect app scope required: ADMIN
OAuth 2.0 scopes required:
ClassicRECOMMENDED:manage:jira-configuration
Granular:delete:issue-type:jira
Request
Path parameters
id Required

string

The ID of the issue type.
Query parameters
alternativeIssueTypeId

string

The ID of the replacement issue type.
Example
Forge
cURL
Node.js
Java
Python
PHP
// This sample uses Atlassian Forge
// https://developer.atlassian.com/platform/forge/
import api, { route } from "@forge/api";

const response = await api.asUser().requestJira(route`/rest/api/2/issuetype/{id}`, {
  method: 'DELETE'
});

console.log(`Response: ${response.status} ${response.statusText}`);
console.log(await response.text());
Responses
204
400
401
403
404
409
423

Returned if the request is successful.
Get alternative issue types

GET /rest/api/2/issuetype/{id}/alternatives

Returns a list of issue types that can be used to replace the issue type. The alternative issue types are those assigned to the same workflow scheme, field configuration scheme, and screen scheme.

This operation can be accessed anonymously.

Permissions required: None.

Connect app scope required: READ
OAuth 2.0 scopes required:
ClassicRECOMMENDED:read:jira-work
Granular:read:issue-type:jira, read:project-category:jira, read:project:jira, read:avatar:jira
Request
Path parameters
id Required

string

The ID of the issue type.
Example
Forge
cURL
Node.js
Java
Python
PHP
// This sample uses Atlassian Forge
// https://developer.atlassian.com/platform/forge/
import api, { route } from "@forge/api";

const response = await api.asUser().requestJira(route`/rest/api/2/issuetype/{id}/alternatives`, {
  headers: {
    'Accept': 'application/json'
  }
});

console.log(`Response: ${response.status} ${response.statusText}`);
console.log(await response.json());
Responses
200
404

Returned if the request is successful.
Content type	Value
application/json	

Array<IssueTypeDetails>

Example response (application/json)
[
  {
    "avatarId": 1,
    "description": "A task that needs to be done.",
    "hierarchyLevel": 0,
    "iconUrl": "https://your-domain.atlassian.net/secure/viewavatar?size=xsmall&avatarId=10299&avatarType=issuetype\",",
    "id": "3",
    "name": "Task",
    "self": "https://your-domain.atlassian.net/rest/api/2/issueType/3",
    "subtask": false
  },
  {
    "avatarId": 10002,
    "description": "A problem with the software.",
    "entityId": "9d7dd6f7-e8b6-4247-954b-7b2c9b2a5ba2",
    "hierarchyLevel": 0,
    "iconUrl": "https://your-domain.atlassian.net/secure/viewavatar?size=xsmall&avatarId=10316&avatarType=issuetype\",",
    "id": "1",
    "name": "Bug",
    "scope": {
      "project": {
        "id": "10000"
      },
      "type": "PROJECT"
    },
    "self": "https://your-domain.atlassian.net/rest/api/2/issueType/1",
    "subtask": false
  }
]
Load issue type avatar

POST /rest/api/2/issuetype/{id}/avatar2

Loads an avatar for the issue type.

Specify the avatar's local file location in the body of the request. Also, include the following headers:

    X-Atlassian-Token: no-check To prevent XSRF protection blocking the request, for more information see Special Headers.
    Content-Type: image/image type Valid image types are JPEG, GIF, or PNG.

For example:
curl --request POST \ --user email@example.com:<api_token> \ --header 'X-Atlassian-Token: no-check' \ --header 'Content-Type: image/< image_type>' \ --data-binary "<@/path/to/file/with/your/avatar>" \ --url 'https://your-domain.atlassian.net/rest/api/2/issuetype/{issueTypeId}'This

The avatar is cropped to a square. If no crop parameters are specified, the square originates at the top left of the image. The length of the square's sides is set to the smaller of the height or width of the image.

The cropped image is then used to create avatars of 16x16, 24x24, 32x32, and 48x48 in size.

After creating the avatar, use Update issue type to set it as the issue type's displayed avatar.

Permissions required: Administer Jira global permission

.

Connect app scope required: ADMIN
OAuth 2.0 scopes required:
ClassicRECOMMENDED:manage:jira-configuration
Granular:write:avatar:jira, write:issue-type:jira, read:avatar:jira
Request
Path parameters
id Required

string

The ID of the issue type.
Query parameters
x

integer

The X coordinate of the top-left corner of the crop region.
Default: 0, Format: int32
y

integer

The Y coordinate of the top-left corner of the crop region.
Default: 0, Format: int32
size Required

integer

The length of each side of the crop region.
Format: int32
Body parameters
Content type	Value
*/*	

anything
Example
Forge
cURL
Node.js
Java
Python
PHP
// This sample uses Atlassian Forge
// https://developer.atlassian.com/platform/forge/
import api, { route } from "@forge/api";

const response = await api.asUser().requestJira(route`/rest/api/2/issuetype/{id}/avatar2?size={size}`, {
  method: 'POST',
  headers: {
    'Accept': 'application/json'
  }
});

console.log(`Response: ${response.status} ${response.statusText}`);
console.log(await response.json());
Responses
201
400
401
403
404

Returned if the request is successful.
Content type	Value
application/json	

Avatar

Example response (application/json)
{
  "id": "1010",
  "isDeletable": true,
  "isSelected": false,
  "isSystemAvatar": false
}


