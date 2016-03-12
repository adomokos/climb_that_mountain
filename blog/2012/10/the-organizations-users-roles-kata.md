## Tuesday, October 2, 2012

### [The Organizations - Users - Roles Kata](http://www.adomokos.com/2012/10/the-organizations-users-roles-kata.html)

We began rewriting one of our applications a couple of months ago. It's a fairly easy app, the only challenge we had so far was replacing our permission based authorization to something more sophisticated saving set up time for our admins. In our legacy app the authorization is controlled through fine grained permission sets. This allows us to do everything we need, but setting it up is a long and tedious process since it does not support role inheritance through organization structures. Thinking more about the business logic I figured other people might like to think about this problem. So here it is:

#### The Organizations - Users - Roles kata

We have three layers of organizations: root organization, organizations and child organizations.

![org_user_role_kata_orgs](/resources/2012/10/org_user_role_kata_orgs.png)

There is only one root organization that we call "Root Org".
Organizations have one parent.
The parent of all organizations is the Root Org.
The organizations can have any number of child organizations, but the child orgs do not have children of their own (they are leaves).

There are three different roles in the system:

*   Admin
*   User
*   Denied

Roles are inherited through the organization hierarchy: an admin to an organization is an admin to all of its child organizations as well. For example - using the organization structure in the diagram above - if I have admin role access to Org 1, than I should have admin access to Child Org 1 and Child Org 2.

If a role is specified to a child org for a given user, that role takes precedence over the inherited role from the organization level.
When I have the "denied" role for Child Org 2, than I only have admin access to Org 1 and Child Org 1 and I don't even see Child Org 2.

![org_user_roles_kata_denied](/resources/2012/10/org_user_roles_kata_denied.png)

Please consider writing code for the logic described above using tests to verify your logic. Simulate the data access code and try to keep the number of queries to a minimum.


POSTED BY ATTILA DOMOKOS AT 10:31 AM

NO COMMENTS
