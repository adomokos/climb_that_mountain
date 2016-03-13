## Wednesday, November 20, 2013

### [Reporting in Rails - with Rails Admin and ...](http://www.adomokos.com/2013/11/reporting-in-rails-with-rails-admin-and.html)

Understanding the data is very important for a business. "How many active customers do I have right now?" "How many did we have last week?" "What are they buying?" "What they are not buying?" "Why?" I could go on and on with these questions that businesses would like to get answered.

I was shadowing one of our customer service folks back in March when someone from sales stopped by and started talking about how great it would be if they could easily export all the candidates for a job and where they applied from. I knew what they were asking for is in the database, I just had to find a way to extract that information.

Our nightly job takes a snapshot of the production database and restores it in our QA environment. This data is good enough for our data mining purposes. The data analyzing tool I was looking for did not have to be very efficient as we can easily restart the QA servers if a long running query would bring it down.

The tool [phpPgAdmin](http://phppgadmin.sourceforge.net/doku.php?id=start) works great, but I wanted to find something more lightweight with Rails and nothing turned up at that time. A few years ago I took a look at the [Rails Admin](https://github.com/sferik/rails_admin) project, I figured that might work for us. I generated a skeleton Rails app, migrated all the DataMapper models to ActiveRecord ones and introduced it to the team shortly after.

They loved it!

The product team was able to see the database tables with the data stored in them without an abstracted reporting layer on top of it. Exporting data as a CSV file was also simple, users used Excel to analyze it further.

The story could end here, but it does not. As the users of Rails Admin wanted to use the tool for intricate queries, we bumped into roadblocks: when the association is more complex between tables, Rails Admin won't work out of the box. I remembered the days when I used phpMyAdmin and how that tool provided a SQL editor tool where I was able to do anything with the data through DML.

We needed something similar and I came up with this tool:

![sql_editor](/resources/2013/11/sql_editor.png)

The users have to put the SQL script in the query textarea, click on the "Run it" button and the result in CSV is pushed back to the client. Since most of the data analysis happens in Excel anyway, I did not bother building a paginated view for the result. I added basic error checking that reports Postgres errors back to the client. I don't restrict this tool for read-only database access as only our product team is using it. If they figure out how to delete the users table through this SQL tool, we can restore the production snapshot in a few minutes.

This is what the logic looks like for the SQL controller:

```rails
require 'csv'

class SqlController < ApplicationController
  before_filter :authenticate_user!

  def editor
    @query = nil
  end

  def run
    @query = params[:query]

    begin
      results = ActiveRecord::Base.connection.execute(@query)
    rescue ActiveRecord::StatementInvalid => ex
      flash.now[:error] = ex.to_s
      render 'editor' and return
    end

    if results.count == 0
      flash.now[:notice] = "No records were found."
      render 'editor' and return
    end

    send_data prepare_csv(results),
              type: 'text/csv; charset=iso-8859-1; header=present',
              disposition: 'attachment;filename=exported_data.csv'
  end

  private

  def prepare_csv(result, options = {})
    CSV.generate(options) do |csv|
      keys = result.first.keys
      csv << keys
      result.each do |row|
        csv << row.values
      end
    end
  end

end
```

We came up with a couple of SQL queries and stored them in a Google Doc. The process is as simple as coping and pasting the query from that Google Doc, changing the where clause for their needs and running the script. I highlighted the data that can be changed in the queries (like date ranges or record IDs) and showed them how it can be adjusted before the query runs.

The product team just can not be happier! We got to the point where this SQL query editor is going to be the primary data retrieval tool. Rails Admin can do the same with simple and straightforward queries but selecting the tables and fields through check boxes is more time consuming than copying a SQL script into the textarea and clicking the button.

Would we ever give this tool to our customers? Of course not! It will always remain a well secured internal tool. But this will be an easy way to figure out what kind of additional reports most of our users want and we will build those into our product as we go. Until then, they can just contact us, and we'll send them the data they need in a few minutes. Win!


POSTED BY ATTILA DOMOKOS AT 9:35 AM


#### 1 comment:

[Mike](https://www.blogger.com/profile/14553450963684936022) said...

_Hopefully this has changed but using ActiveRecord::Base.connection.execute opens you up to SQL injection attacks. It would be better to setup parameterized queries. Since you're already using ActiveRecord it wouldn't be that much more work._

November 13, 2015 at 10:16 AM
