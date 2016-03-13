## Thursday, February 7, 2013

### [User Devised - Disconnecting Your User Entity From Devise](http://www.adomokos.com/2013/02/user-devised-disconnecting-your-user.html)

We have several Rails apps with duplicated user information. If you're John Smith in one of the apps, we will create a new user with the same information in the others. That's crazy, single sign-on should be the obvious solution.

In order to get there we had to disconnect the authentication mechanism from our User entity. Once we have the same separation in our apps, pulling the authentication out into a service or API should be very easy.
Also, I like to keep the User model application specific. I would much rather have fields that are relevant to our app needs in our User model and not mix devise's fields into that entity.
Here is what I had to do to disconnect [devise](https://github.com/plataformatec/devise) from our User model.

First of all, I had to generate a member table with the following migration:

```ruby
class AddMembers < ActiveRecord::Migration
  def change
    create_table(:members) do |t|
      ## Database authenticatable
      t.integer :user_id
      t.string :email,              :null => false, :default => ""
      t.string :encrypted_password, :null => false, :default => ""
      t.string :first_name,         :default => ""
      t.string :last_name,          :default => ""
      t.string :middle_name,        :default => ""

      ## Recoverable
      t.string   :reset_password_token
      t.datetime :reset_password_sent_at

      ## Rememberable
      t.datetime :remember_created_at

      ## Trackable
      t.integer  :sign_in_count, :default => 0
      t.datetime :current_sign_in_at
      t.datetime :last_sign_in_at
      t.string   :current_sign_in_ip
      t.string   :last_sign_in_ip

      ## Confirmable
      t.string   :confirmation_token
      t.datetime :confirmed_at
      t.datetime :confirmation_sent_at
      # Only if using reconfirmable
      t.string   :unconfirmed_email 

      ## Lockable
      # Only if lock strategy is :failed_attempts
      t.integer  :failed_attempts, :default => 0 
      t.string   :unlock_token
      # Only if unlock strategy is :email or :both
      t.datetime :locked_at

      ## Token authenticatable
      t.string :authentication_token

      # Uncomment below if timestamps were not included
      # in your original model.
      t.timestamps
    end

    add_index :members, :email,                :unique => true
    add_index :members, :reset_password_token, :unique => true
    add_index :members, :confirmation_token,   :unique => true
    add_index :members, :unlock_token,         :unique => true
    add_index :members, :authentication_token, :unique => true
  end

  def self.down
    # By default, we don't want to make any assumption
    # about how to roll back a migration when your
    # model already existed. Please edit below which fields
    # you would like to remove in this migration.
    raise ActiveRecord::IrreversibleMigration
  end
end
```

Instead of putting all the Member related code into the User model, I created a concern that can be mixed into the User model. This module can be reused in all of the Rails apps to connect the Member entity to the User.

```ruby
require 'active_support/concern'
require 'active_support/core_ext/module'

module ModelExtensions
  module UserDevised
    extend ActiveSupport::Concern

    included do
      has_one :member
      validates_associated :member

      after_save :save_member, :if => lambda {|u| u.member }
      delegate :last_sign_in_at, :password, :password=,
               :password_confirmation, :password_confirmation=,
               :to => :member

      before_validation do
        self.member.first_name = self.first_name
        self.member.middle_name = self.middle_name
        self.member.last_name = self.last_name
        self.member.email = self.email_address
      end
    end

    def initialize(*params)
      super(*params)
      self.build_member(*params) if member.nil?
    end

    def save_member
      self.member.save!
    end
  end
end
```

I'd like to mention a couple of things about the module you see above. I am using one-to-one relationship between User and Member. Whenever I save a User, I save the Member as well by using the `after_save` callback. Admins can overwrite the users' password, I delegate those fields from User to Member, so the Member entity remains hidden behind the User. When a new User is initialized, I create a Member entity as well as you see it in the `initialize` method.

Devise strategies and overrides are defined in the Member model.

```ruby
class Member < ActiveRecord::Base
  belongs_to :user

  # Include default devise modules. Others available are:
  # :token_authenticatable, :confirmable,
  # :lockable, :timeoutable and :omniauthable

  devise :database_authenticatable, :lockable, :timeoutable,
         :recoverable, :trackable, :validatable, :omniauthable
         # non-registerable :registerable, :rememberable

  validates :first_name, :last_name, :presence => true

  def active_for_authentication?
    # Comment out the below debug statement to view
    # the properties of the returned self model values.

    super && user.active?
  end

  def confirmed_account?
    (self.last_sign_in_at.nil? == false &&
        self.reset_password_token.nil?)
  end

end
```

I remember how hard it was to find information on the separation of the application specific User and the devise Member entity when I was working on this. I hope someone will find this code helpful, it would have helped me a lot.


POSTED BY ATTILA DOMOKOS AT 2:34 PM


NO COMMENTS
