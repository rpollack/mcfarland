# McFARLAND ⚾🤖

**Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data**

An AI-powered baseball analytics application that provides intelligent performance analysis for both hitters and pitchers, comparing current season stats to historical averages with personality-driven commentary.

## Features

### 🎯 Core Analytics
- **Dual Player Support**: Comprehensive analysis for both hitters and pitchers
- **Historical Comparison**: 2025 current season stats vs. 2022-2024 averages
- **Smart Metrics**: Focus on skill vs. luck indicators (BABIP, xwOBA, barrel rates, etc.)
- **Performance Forecasting**: AI-powered predictions for rest-of-season performance

### 🤖 AI-Powered Analysis
- **Multiple Personalities**: Choose from 7 different analysis styles:
  - Straightforward (default)
  - Analytics Dork (sabermetrics-focused)
  - Old Coot (traditional baseball wisdom)
  - Gen Z (modern slang and references)
  - 1970s Fan (old-school baseball perspective)
  - Sensationalist (dramatic sports journalism)
  - Shakespeare (iambic pentameter analysis!)

### 📊 Data Visualization
- **Player Trend Charts**: Visual comparison of current vs. historical performance
- **MLB Player Photos**: Official headshots with intelligent fallbacks
- **Responsive Design**: Optimized for desktop and mobile devices

### ⚡ Performance Features
- **Intelligent API Caching**: Reduces costs and improves response times
- **Daily Data Refresh**: Automated GitHub Actions update player statistics
- **Real-time Loading**: Progress indicators and smooth user experience

## Data Sources

- **Player Statistics**: [FanGraphs](https://www.fangraphs.com/) leaderboards and API
- **Player Photos**: MLB official headshots with FanGraphs fallbacks
- **Historical Data**: 2022-2024 CSV files for baseline comparisons

## Technical Stack

### Backend
- **R/Shiny**: Core application framework
- **tidyverse**: Data manipulation and analysis
- **baseballr**: Baseball data access
- **httr/jsonlite**: API interactions
- **ggplot2**: Data visualization

### Frontend
- **bslib**: Modern Bootstrap theming
- **shinybusy**: Loading animations
- **commonmark**: Markdown rendering
- **Glass morphism design**: Modern UI with backdrop filters

### AI Integration
- **OpenAI GPT-4.1**: Intelligent performance analysis
- **Custom prompts**: Tailored analysis based on player type and metrics
- **Response caching**: Memory-based caching for improved performance

## Installation & Setup

### Prerequisites
```r
# Required R packages
install.packages(c(
  "shiny", "dplyr", "readr", "purrr", "stringr",
  "httr", "jsonlite", "bslib", "commonmark", 
  "shinybusy", "ggplot2", "htmltools", "digest",
  "tidyverse", "baseballr", "stringi", "janitor"
))
```

### Environment Variables
```bash
# Required for AI analysis
export OPENAI_API_KEY="your_openai_api_key_here"
```

### Running the Application

#### Local Development
```r
# Clone the repository
git clone https://github.com/your-username/leadRboard.git
cd leadRboard

# Run the Shiny app
shiny::runApp("app/app.R")
```

#### Data Refresh
```r
# Manual data refresh (or use GitHub Actions)
source("refresh_data.R")
```

## Data Pipeline

### Automated Daily Refresh
The application uses GitHub Actions to automatically refresh data daily at 6 AM EST:

```yaml
# .github/workflows/refresh-data.yml
name: Daily Baseball Data Refresh
on:
  schedule:
    - cron: '0 11 * * *'  # 6 AM EST
  workflow_dispatch:  # Manual trigger
```

### Data Processing
1. **Hitter Metrics**: AVG, OBP, SLG, K%, BB%, Barrel%, BABIP, wOBA, xwOBA
2. **Pitcher Metrics**: ERA, xERA, K%, BB%, CSW%, Barrel%, BABIP, LOB%
3. **Difference Calculation**: Current year vs. 3-year historical average
4. **Player Lookup**: Unified table with compound IDs for dual-role players

## Key Files Structure

```
├── app/
│   └── app.R                 # Main Shiny application
├── .github/workflows/
│   └── refresh-data.yml      # Automated data refresh
├── refresh_data.R            # Data processing script
├── full_stats_hitters.csv    # Current hitter statistics
├── full_stats_pitchers.csv   # Current pitcher statistics  
├── player_lookup.csv         # Player directory with IDs
├── fangraphs-leaderboards-*.csv  # Historical hitter data
└── pitcher-stats-*.csv       # Historical pitcher data
```

## API Usage & Caching

### Smart Caching System
- **Memory-based caching**: Stores API responses for 1 hour
- **Cache size management**: Automatically maintains 25-50 recent entries
- **Performance tracking**: Monitors cache hits vs. API calls
- **Cost optimization**: Reduces OpenAI API usage significantly

### Analysis Generation
The AI analysis incorporates:
- **Skill vs. Luck Indicators**: Distinguishes sustainable performance changes
- **Age Considerations**: Adjusts expectations based on player age
- **Sample Size Awareness**: Accounts for statistical significance
- **Position-specific Insights**: Tailored analysis for SP/RP pitchers

## Usage Examples

### Basic Player Analysis
1. Select a player from the dropdown
2. Choose an analysis personality/vibe
3. View AI-generated analysis with trend visualization

### Understanding the Analysis
- **Green trends**: Generally positive performance indicators
- **Statistical significance**: Larger sample sizes = more reliable trends
- **Luck indicators**: BABIP, LOB%, ERA-xERA gaps highlight variance
- **Skill indicators**: K%, BB%, Barrel% show true talent changes

## Contributing

### Data Updates
- Historical data files should be updated annually
- FanGraphs API integration handles current season automatically
- Player ID mapping requires occasional maintenance

### Feature Requests
Submit feedback and feature requests via the in-app forms or GitHub issues.

## Version History

- **v0.8**: Intelligent API caching system
- **v0.7**: MLB player headshots integration
- **v0.6**: Pitcher analysis support
- **v0.5**: Notification signup system
- **v0.4**: Enhanced metrics (Barrels/PA, xwOBA gaps)
- **v0.3**: Player trend visualizations
- **v0.2**: Shakespeare analysis mode
- **v0.1**: Initial hitter-only version

## Acknowledgments

- **Data**: [FanGraphs](https://www.fangraphs.com/) for comprehensive baseball statistics
- **Photos**: MLB for official player headshots
- **Inspiration**: [T.J. McFarland](https://www.fangraphs.com/players/tj-mcfarland/3237/stats?position=P) - the utility pitcher who inspired the name
- **AI**: OpenAI GPT-4.1 for intelligent analysis generation

## License

This project is for educational and personal use. Please respect FanGraphs' terms of service and MLB's image usage policies.

---

**Built with ❤️ and ⚾ by Ryan Pollack**
