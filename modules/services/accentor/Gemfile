source 'https://rubygems.org'
git_source(:github) { |repo| "https://github.com/#{repo}.git" }

ruby '~> 3.1.0'

# Bundle edge Rails instead: gem 'rails', github: 'rails/rails'
gem 'rails', '~> 7.0'
# Use postgresql as the database for Active Record
gem 'pg', '>= 1.2.3', '< 2.0'
# Use Puma as the app server
gem 'puma', '~> 5.6'
# Use ActiveModelSerializer for serializing to JSON
gem 'active_model_serializers', '~> 0.10'
# Use ActiveModel has_secure_password
gem 'bcrypt', '~> 3.1'

gem 'delayed_job_active_record', '~> 4.1'

gem 'delayed_cron_job', '~> 0.9'

gem 'wahwah', '~> 1.3.0'

gem 'has_scope', '~> 0.8'
gem 'pundit', '~> 2.2'
gem 'will_paginate', '~> 3.3'

# Use ActiveStorage variant
gem 'image_processing', '~> 1.12.2'

# Reduces boot times through caching; required in config/boot.rb
gem 'bootsnap', '~> 1.11.1', require: false

# Use Rack CORS for handling Cross-Origin Resource Sharing (CORS), making cross-origin AJAX possible
gem 'rack-cors', '~> 1.1'

# Windows does not include zoneinfo files, so bundle the tzinfo-data gem
gem 'tzinfo-data', platforms: %i[mingw mswin x64_mingw jruby]

group :test do
  gem 'codecov', '~> 0.6.0', require: false
  gem 'mocha', '~> 1.13.0', require: false
  gem 'simplecov', '~> 0.21', require: false
end

group :development, :test do
  gem 'factory_bot_rails', '~> 6.2'
  gem 'faker', '~> 2.20'

  # Call 'byebug' anywhere in the code to stop execution and get a debugger console
  gem 'byebug', platforms: %i[mri mingw x64_mingw]
end

group :development do
  gem 'annotate', '~> 3.2' # Remove workaround in lib/tasks/annotate.rb when https://github.com/ctran/annotate_models/issues/696 is fixed
  gem 'rubocop-minitest', '~> 0.17.2'
  gem 'rubocop-rails', '~> 2.13'
end
