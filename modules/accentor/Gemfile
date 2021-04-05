source 'https://rubygems.org'
git_source(:github) { |repo| "https://github.com/#{repo}.git" }

ruby '2.7.2'

# Bundle edge Rails instead: gem 'rails', github: 'rails/rails'
gem 'rails', '~> 6.1'
# Keep this low enough until https://github.com/rack/rack/issues/1619 is fixed
gem 'rack', '~> 2.1.4'
# Use postgresql as the database for Active Record
gem 'pg', '>= 1.2.3', '< 2.0'
# Use Puma as the app server
gem 'puma', '~> 5.2'
# Use ActiveModelSerializer for serializing to JSON
gem 'active_model_serializers', '~> 0.10'
# Use Redis adapter to run Action Cable in production
# gem 'redis', '~> 4.0'
# Use ActiveModel has_secure_password
gem 'bcrypt', '~> 3.1'

gem 'delayed_job_active_record', '~> 4.1'

gem 'delayed_cron_job', '~> 0.7'

gem 'wahwah', '~> 1.1.1'

gem 'has_scope', '~> 0.8'
gem 'pundit', '~> 2.1'
gem 'will_paginate', '~> 3.3'

# Use ActiveStorage variant
gem 'image_processing', '~> 1.12.1'

# Reduces boot times through caching; required in config/boot.rb
gem 'bootsnap', '>= 1.4.6', require: false

# Use Rack CORS for handling Cross-Origin Resource Sharing (CORS), making cross-origin AJAX possible
gem 'rack-cors', '~> 1.1'

group :test do
  gem 'codecov', '~> 0.5.1', require: false
  gem 'mocha', '~> 1.12.0', require: false
  gem 'simplecov', '~> 0.21', require: false
end

group :development, :test do
  gem 'factory_bot_rails', '~> 6.1'
  gem 'faker', '~> 2.17'

  # Call 'byebug' anywhere in the code to stop execution and get a debugger console
  gem 'byebug', platforms: %i[mri mingw x64_mingw]
end

group :development do
  gem 'annotate', '~> 3.1'

  gem 'rubocop-minitest', '~> 0.11.1'
  gem 'rubocop-rails', '~> 2.9'

  gem 'listen', '>= 3.1.5', '< 3.6'
  # Spring speeds up development by keeping your application running in the background. Read more: https://github.com/rails/spring
  gem 'spring', '~> 2.1'
  gem 'spring-watcher-listen', '~> 2.0'
end

# Windows does not include zoneinfo files, so bundle the tzinfo-data gem
gem 'tzinfo-data', platforms: %i[mingw mswin x64_mingw jruby]
