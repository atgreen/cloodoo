#!/bin/bash
# populate-screenshot-data.sh
#
# Populates a temporary Cloodoo database with fake data for screenshots.
# Usage: CLOODOO_HOME=/tmp/cloodoo-screenshots ./populate-screenshot-data.sh

set -e

# Check if CLOODOO_HOME is set
if [ -z "$CLOODOO_HOME" ]; then
    echo "Error: CLOODOO_HOME must be set to a temporary directory"
    echo "Usage: CLOODOO_HOME=/tmp/cloodoo-screenshots $0"
    exit 1
fi

# Create the directory
mkdir -p "$CLOODOO_HOME"

CLOODOO="./cloodoo"

echo "Populating database at: $CLOODOO_HOME"
echo "================================================"

# High priority tasks - Work
echo "Adding high priority work tasks..."
$CLOODOO add "Review quarterly financial report for board meeting" --priority high --due "$(date -d '+2 days' +%Y-%m-%d)" --tag work --tag urgent --tag finance
$CLOODOO add "Fix production server SSL certificate expiring today" --priority high --due "$(date +%Y-%m-%d)" --tag work --tag devops --tag urgent
$CLOODOO add "Complete client proposal for ABC Corp" --priority high --due "$(date -d '+3 days' +%Y-%m-%d)" --note "Include pricing, timeline, team bios, and past work samples" --tag work --tag sales --tag urgent
$CLOODOO add "Deploy security patches to production servers" --priority high --due "$(date +%Y-%m-%d)" --tag work --tag devops --tag security
$CLOODOO add "Resolve critical bug in payment processing" --priority high --due "$(date +%Y-%m-%d)" --tag work --tag development --tag urgent
$CLOODOO add "Prepare Q1 investor presentation" --priority high --due "$(date -d '+5 days' +%Y-%m-%d)" --note "Need revenue growth, user metrics, and expansion plans" --tag work --tag executive
$CLOODOO add "Interview senior engineer candidates" --priority high --due "$(date -d '+4 days' +%Y-%m-%d)" --tag work --tag hiring

# High priority - Personal/Health
echo "Adding high priority personal tasks..."
$CLOODOO add "Call dentist for emergency appointment" --priority high --due "$(date +%Y-%m-%d)" --tag health --tag urgent
$CLOODOO add "Renew passport before expiration" --priority high --due "$(date -d '+10 days' +%Y-%m-%d)" --tag personal --tag travel
$CLOODOO add "Schedule car inspection (expires this week)" --priority high --due "$(date -d '+5 days' +%Y-%m-%d)" --tag personal --tag car

# Medium priority tasks - Work
echo "Adding medium priority work tasks..."
$CLOODOO add "Schedule team meeting for Q2 planning" --priority medium --due "$(date -d '+7 days' +%Y-%m-%d)" --tag work --tag meetings
$CLOODOO add "Reply to client emails from yesterday" --priority medium --due "$(date +%Y-%m-%d)" --tag work --tag email
$CLOODOO add "Review pull requests for authentication module" --priority medium --tag work --tag development
$CLOODOO add "Update employee handbook with new policies" --priority medium --due "$(date -d '+2 weeks' +%Y-%m-%d)" --tag work --tag hr
$CLOODOO add "Conduct performance reviews for team members" --priority medium --due "$(date -d '+10 days' +%Y-%m-%d)" --tag work --tag management
$CLOODOO add "Plan company offsite for next quarter" --priority medium --due "$(date -d '+3 weeks' +%Y-%m-%d)" --tag work --tag events
$CLOODOO add "Set up CI/CD pipeline for mobile app" --priority medium --tag work --tag devops --tag mobile
$CLOODOO add "Write blog post about new product features" --priority medium --due "$(date -d '+1 week' +%Y-%m-%d)" --tag work --tag marketing
$CLOODOO add "Analyze user feedback from last sprint" --priority medium --tag work --tag product --tag analytics
$CLOODOO add "Negotiate contract renewal with vendor" --priority medium --due "$(date -d '+2 weeks' +%Y-%m-%d)" --tag work --tag legal
$CLOODOO add "Prepare documentation for API v2 release" --priority medium --due "$(date -d '+1 week' +%Y-%m-%d)" --tag work --tag documentation
$CLOODOO add "Review security audit findings" --priority medium --due "$(date -d '+5 days' +%Y-%m-%d)" --tag work --tag security
$CLOODOO add "Update onboarding materials for new hires" --priority medium --tag work --tag hr
$CLOODOO add "Configure monitoring alerts for production" --priority medium --tag work --tag devops
$CLOODOO add "Research competitor products and features" --priority medium --tag work --tag research

# Medium priority - Personal
echo "Adding medium priority personal tasks..."
$CLOODOO add "Buy groceries for the week" --priority medium --due "$(date -d '+1 day' +%Y-%m-%d)" --tag shopping --tag food
$CLOODOO add "Research vacation destinations for summer" --priority medium --tag personal --tag travel
$CLOODOO add "Plan birthday party for Sarah" --priority medium --due "$(date -d '+2 weeks' +%Y-%m-%d)" --note "Reserve restaurant for 8pm, invite 15 people, order cake" --tag family --tag events
$CLOODOO add "Schedule annual physical checkup" --priority medium --tag health --tag doctors
$CLOODOO add "Get oil change for the car" --priority medium --due "$(date -d '+1 week' +%Y-%m-%d)" --tag car --tag maintenance
$CLOODOO add "Donate old clothes to charity" --priority medium --tag home --tag organizing
$CLOODOO add "Call mom to wish happy anniversary" --priority medium --due "$(date -d '+8 days' +%Y-%m-%d)" --tag family
$CLOODOO add "Renew gym membership" --priority medium --due "$(date -d '+12 days' +%Y-%m-%d)" --tag health --tag fitness
$CLOODOO add "Buy wedding gift for colleague" --priority medium --due "$(date -d '+2 weeks' +%Y-%m-%d)" --tag shopping --tag gifts
$CLOODOO add "Register kids for summer camp" --priority medium --due "$(date -d '+3 weeks' +%Y-%m-%d)" --tag family --tag kids
$CLOODOO add "Update emergency contact information" --priority medium --tag personal --tag administrative
$CLOODOO add "Schedule dentist cleaning appointment" --priority medium --due "$(date -d '+2 weeks' +%Y-%m-%d)" --tag health --tag dental
$CLOODOO add "Find plumber for bathroom renovation" --priority medium --tag home --tag renovation
$CLOODOO add "Research retirement investment options" --priority medium --tag finance --tag investing
$CLOODOO add "Book hotel for cousin's wedding" --priority medium --due "$(date -d '+3 weeks' +%Y-%m-%d)" --tag travel --tag family

# Low priority tasks - Work
echo "Adding low priority work tasks..."
$CLOODOO add "Organize desk and filing cabinet" --priority low --tag work --tag organizing
$CLOODOO add "Update project documentation on wiki" --priority low --tag work --tag documentation
$CLOODOO add "Clean out email inbox and archive old messages" --priority low --tag work --tag email
$CLOODOO add "Update LinkedIn profile with recent accomplishments" --priority low --tag work --tag networking
$CLOODOO add "Research new project management tools" --priority low --tag work --tag tools
$CLOODOO add "Sort through old project files and archive" --priority low --tag work --tag organizing
$CLOODOO add "Update contact information in company directory" --priority low --tag work --tag administrative
$CLOODOO add "Learn new design patterns from online course" --priority low --tag work --tag learning
$CLOODOO add "Refactor legacy code in authentication module" --priority low --tag work --tag development
$CLOODOO add "Set up development environment on new laptop" --priority low --tag work --tag setup
$CLOODOO add "Review and organize browser bookmarks" --priority low --tag work --tag organizing
$CLOODOO add "Update team wiki with recent decisions" --priority low --tag work --tag documentation
$CLOODOO add "Research new testing frameworks" --priority low --tag work --tag research
$CLOODOO add "Organize Slack channels and archive old ones" --priority low --tag work --tag communication
$CLOODOO add "Clean up unused cloud storage files" --priority low --tag work --tag cleanup

# Low priority - Personal
echo "Adding low priority personal tasks..."
$CLOODOO add "Read 'Atomic Habits' book" --priority low --tag personal --tag reading
$CLOODOO add "Organize photo albums and backup to cloud" --priority low --tag home --tag organizing
$CLOODOO add "Clean out garage and basement" --priority low --tag home --tag cleaning
$CLOODOO add "Research home automation options" --priority low --tag home --tag technology
$CLOODOO add "Update personal website portfolio" --priority low --tag personal --tag website
$CLOODOO add "Sort through old magazines and recycle" --priority low --tag home --tag declutter
$CLOODOO add "Plant herbs in kitchen garden" --priority low --due "$(date -d '+2 weeks' +%Y-%m-%d)" --tag home --tag gardening
$CLOODOO add "Research energy-efficient appliances" --priority low --tag home --tag shopping
$CLOODOO add "Learn basic Spanish phrases for trip" --priority low --tag personal --tag learning
$CLOODOO add "Organize recipe collection and digitize favorites" --priority low --tag home --tag cooking
$CLOODOO add "Sort through kids' old toys and donate" --priority low --tag home --tag organizing
$CLOODOO add "Update address book with new contacts" --priority low --tag personal --tag organizing
$CLOODOO add "Research podcast topics to listen to" --priority low --tag personal --tag entertainment
$CLOODOO add "Organize digital files and folders" --priority low --tag personal --tag digital
$CLOODOO add "Plan home improvement projects for spring" --priority low --tag home --tag planning

# Overdue tasks
echo "Adding overdue tasks..."
$CLOODOO add "Submit quarterly expense report to accounting" --priority medium --due "$(date -d '-3 days' +%Y-%m-%d)" --tag work --tag finance
$CLOODOO add "Return library books (late fees accumulating)" --priority low --due "$(date -d '-1 week' +%Y-%m-%d)" --tag personal --tag errands
$CLOODOO add "Send thank you notes for birthday gifts" --priority medium --due "$(date -d '-5 days' +%Y-%m-%d)" --tag personal --tag correspondence
$CLOODOO add "Respond to vendor quote request" --priority medium --due "$(date -d '-2 days' +%Y-%m-%d)" --tag work --tag procurement
$CLOODOO add "Submit reimbursement for conference expenses" --priority medium --due "$(date -d '-4 days' +%Y-%m-%d)" --tag work --tag finance
$CLOODOO add "File warranty claim for broken appliance" --priority medium --due "$(date -d '-1 week' +%Y-%m-%d)" --tag home --tag warranty

# Tasks with detailed descriptions
echo "Adding tasks with detailed descriptions..."
$CLOODOO add "Migrate database to new server infrastructure" --priority high --due "$(date -d '+1 week' +%Y-%m-%d)" --note "Plan downtime window, backup all data, test migration scripts, coordinate with DevOps team. Estimated 4-hour maintenance window." --tag work --tag devops --tag database
$CLOODOO add "Research and compare health insurance plans" --priority medium --due "$(date -d '+3 weeks' +%Y-%m-%d)" --note "Open enrollment ends soon. Compare premiums, deductibles, and coverage for family. Check if current doctors are in-network." --tag health --tag insurance --tag finance
$CLOODOO add "Organize family reunion for summer" --priority medium --due "$(date -d '+1 month' +%Y-%m-%d)" --note "Book venue for 50 people, arrange catering, send invitations, create activity schedule. Consider park pavilion or community center." --tag family --tag events
$CLOODOO add "Set up automated testing for API endpoints" --priority medium --note "Write unit tests, integration tests, and end-to-end tests. Set up GitHub Actions workflow. Target 80% code coverage." --tag work --tag development --tag testing
$CLOODOO add "Plan weekend camping trip to state park" --priority low --due "$(date -d '+3 weeks' +%Y-%m-%d)" --note "Reserve campsite, check weather forecast, pack camping gear, plan meals, print trail maps" --tag personal --tag outdoor --tag travel

# Completed tasks (for completed view)
echo "Adding completed tasks..."
$CLOODOO add "Complete annual tax filing for 2025" --priority high --tag finance --tag personal --tag taxes
$CLOODOO add "Renew car insurance policy" --priority medium --tag finance --tag car --tag insurance
$CLOODOO add "Backup important files to cloud storage" --priority medium --tag work --tag technology
$CLOODOO add "Submit timesheet for last week" --priority high --tag work --tag administrative
$CLOODOO add "Schedule annual performance review" --priority medium --tag work --tag hr
$CLOODOO add "Order printer toner cartridges" --priority low --tag work --tag supplies
$CLOODOO add "Install security updates on laptop" --priority high --tag work --tag security
$CLOODOO add "Cancel unused subscription services" --priority low --tag finance --tag subscriptions
$CLOODOO add "Clean out refrigerator" --priority low --tag home --tag cleaning
$CLOODOO add "Return defective keyboard to Amazon" --priority medium --tag shopping --tag returns
$CLOODOO add "Schedule vet appointment for dog" --priority medium --tag pets --tag health
$CLOODOO add "Pay utility bills for this month" --priority high --tag finance --tag bills
$CLOODOO add "Update resume with recent projects" --priority low --tag personal --tag career
$CLOODOO add "Fix leaky faucet in bathroom" --priority medium --tag home --tag maintenance
$CLOODOO add "Attend networking event at tech conference" --priority medium --tag work --tag networking

# Mark completed tasks
echo "Marking tasks as completed..."
TASK_IDS=$($CLOODOO list --format json 2>/dev/null | grep -o '"id":"[^"]*"' | tail -15 | cut -d'"' -f4 || echo "")
if [ -n "$TASK_IDS" ]; then
    for id in $TASK_IDS; do
        $CLOODOO complete "$id" 2>/dev/null || true
    done
    echo "Marked ~15 tasks as completed"
else
    echo "Warning: Could not retrieve task IDs for completion"
fi

echo ""
echo "================================================"
echo "Screenshot database populated successfully!"
echo ""
echo "Total tasks created: ~85"
echo "  - High priority: 10"
echo "  - Medium priority: 40"
echo "  - Low priority: 30"
echo "  - Overdue: 6"
echo "  - Completed: 15"
echo ""
echo "To use this database:"
echo "  export CLOODOO_HOME=$CLOODOO_HOME"
echo "  ./cloodoo"
echo ""
echo "To launch TUI with this data:"
echo "  CLOODOO_HOME=$CLOODOO_HOME ./cloodoo"
echo ""
echo "To clean up:"
echo "  rm -rf $CLOODOO_HOME"
