require 'capistrano/ext/multistage'

# User to deploy with
set :user, 'deploy'
set :group, "www"

set :stages, %w{staging, production}
set :default_stage, "staging"
set :application, "elli_template"

# Repository to fetch our sources from
set :scm, :git
set :repository, 'git@github.com:diegoeche/elli_template.git'
set :branch, 'master'
set :deploy_via, :remote_cache

# Disable asset timestamps updates,
set :normalize_asset_timestamps, false

set(:deploy_to) { "/var/www/#{application}" }

set :default_environment, {
  'PATH' => "/usr/local/bin:${PATH}"
}

# Prevent deploying to critical environments by mistake
before 'deploy:update' do
  if [:production].include?(stage)
    quiz
  end
end

after 'deploy:setup' do
  # Correct permissions in case necessary
  sudo "chown -R #{user} #{deploy_to}"
  sudo "chgrp -R #{group} #{deploy_to}"
end

def env_run(base_dir, cmd)
  run "cd #{base_dir}; #{cmd}"
end

# We might not use this rebar, but it's a nice to have
def rebar(cmd)
  env_run current_release, "script/rebar #{cmd}"
end

# Issue an application control command.
def control(cmd)
  env_run "#{deploy_to}/current", "ERL_ENV=#{stage} script/control #{cmd}"
end

after 'deploy:create_symlink' do
  # Fetch and compile the dependenciesca
  run "cd #{current_release}; rebar get-deps"
  run "cd #{current_release}; rebar compile"
end

namespace :deploy do
  task :restart do
    control "stop"
    control "start"
  end
  task :start do
    control "start"
  end
  task :stop do
    control "stop"
  end
end

# Simple math to check production deployment
def quiz
  puts "\n \x1b[1;31m * WARNING: deploying to #{stage} * \x1b[0;0m\n"
  a = rand(5)
  b = rand(5)
  puts "Solve this puzzle before deploying"
  puts "#{a} + #{b} ="
  c = STDIN.gets.chomp.to_i
  while c != a + b
    puts "\n#{a} + #{b} ="
    c = STDIN.gets.chomp.to_i
  end
end
