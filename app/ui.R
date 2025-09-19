# UI Definition -------------------------------------------------------------

# UI Definition - Redesigned with Progressive Flow
ui <- page_navbar(
  title = "McFARLAND",
  header = tagList(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0, user-scalable=no, viewport-fit=cover, shrink-to-fit=no"),
    tags$style(ui_styles), # Keep your existing styles
    tags$script(HTML("
      var heartbeatInterval;
      function startHeartbeat() {
        heartbeatInterval = setInterval(function() {
          Shiny.setInputValue('heartbeat', Date.now(), {priority: 'event'});
        }, 15000);
      }
      function stopHeartbeat() {
        if (heartbeatInterval) {
          clearInterval(heartbeatInterval);
          heartbeatInterval = null;
        }
      }
      document.addEventListener('visibilitychange', function() {
        if (document.visibilityState === 'visible') {
          startHeartbeat();
        } else {
          stopHeartbeat();
        }
      });
      if (document.visibilityState === 'visible') {
        startHeartbeat();
      }
      $(document).on('shiny:disconnected', function() {
        setTimeout(function(){ location.reload(); }, 3000);
      });
    ")), 
    tags$script(HTML("
      (function() {
        function flagIosSafari() {
          var ua = window.navigator.userAgent || '';
          var isIOS = /iP(hone|ad|od)/.test(ua);
          var isWebKit = /WebKit/.test(ua);
          var isCriOS = /CriOS/.test(ua);
          var isFxiOS = /FxiOS/.test(ua);
          if (!(isIOS && isWebKit && !isCriOS && !isFxiOS)) {
            return;
          }
          if (!document.body) {
            document.addEventListener('DOMContentLoaded', flagIosSafari);
            return;
          }
          document.documentElement.classList.add('ios-safari');
          document.body.classList.add('ios-safari');
        }
        if (document.readyState !== 'loading') {
          flagIosSafari();
        } else {
          document.addEventListener('DOMContentLoaded', flagIosSafari);
        }
      })();
    ")), 
    # Add new styles for the progressive flow
    tags$style(HTML("
      /* Progressive Flow Specific Styles */
      .hero-section {
        text-align: center;
        color: white;
        margin: 1rem 0 1rem 0;
        padding: 0 1rem;
      }

      .hero-title {
        font-size: 2.5rem;
        font-weight: 700;
        margin-bottom: 0.5rem;
        text-shadow: 0 2px 4px rgba(0,0,0,0.3);
      }

      .hero-subtitle {
        font-size: 1.1rem;
        opacity: 0.9;
        margin-bottom: 0;
      }

      .instruction-alert {
        background-color: rgba(255,255,255,0.15);
        color: #fff;
        border: 1px solid rgba(255,255,255,0.25);
        margin: 0 1rem 1rem;
      }

      .instruction-steps {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 0.25rem;
        text-align: center;
      }

      .instruction-steps span {
        white-space: nowrap;
      }

      .search-card {
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(20px);
        border-radius: 20px;
        padding: 2rem;
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.1);
        margin-bottom: 2rem;
      }

      .search-input-container .selectize-input {
        border: 2px solid rgba(46, 134, 171, 0.2);
        border-radius: 15px;
        padding: 1rem 1.5rem;
        font-size: 1.1rem;
        background: rgba(255, 255, 255, 0.9);
      }

      /* Hide default selectize dropdown arrow for pure typeahead */
      .search-input-container .selectize-control.single .selectize-input:after {
        display: none;
      }

      .step-card {
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(20px);
        border-radius: 15px;
        padding: 1.5rem;
        box-shadow: 0 10px 25px rgba(0, 0, 0, 0.1);
        transition: all 0.3s ease;
        border-left: 4px solid transparent;
        margin-bottom: 1.5rem;
      }

      .step-card.active {
        border-left-color: #2E86AB;
        transform: translateY(-2px);
        box-shadow: 0 15px 35px rgba(0, 0, 0, 0.15);
      }

      .step-card.inactive {
        opacity: 0.6;
        border-left-color: #dee2e6;
      }

      .step-header {
        display: flex;
        align-items: center;
        gap: 1rem;
        margin-bottom: 1rem;
      }

      .step-number {
        background: linear-gradient(135deg, #2E86AB, #4A90E2);
        color: white;
        width: 40px;
        height: 40px;
        border-radius: 50%;
        display: flex;
        align-items: center;
        justify-content: center;
        font-weight: 700;
        font-size: 1.1rem;
        flex-shrink: 0;
      }

      .step-number.inactive {
        background: #dee2e6;
        color: #6c757d;
      }

      .step-title {
        font-size: 1.3rem;
        font-weight: 600;
        color: #2E86AB;
        margin: 0;
      }

      .step-title.inactive {
        color: #6c757d;
      }

      .player-preview {
        display: flex;
        align-items: center;
        gap: 1rem;
        padding: 1rem;
        background: rgba(46, 134, 171, 0.05);
        border-radius: 12px;
        margin-bottom: 1rem;
      }

      .player-preview-avatar {
        width: 60px;
        height: 60px;
        border-radius: 50%;
        border: 2px solid #2E86AB;
        object-fit: cover;
      }

      .player-preview-info {
        flex: 1;
      }

      .player-preview-info h4 {
        margin: 0;
        color: #2E86AB;
        font-weight: 600;
      }

      .player-preview-info p {
        margin: 0;
        color: #6c757d;
        font-size: 0.9rem;
      }


/* Compact Vibe Selector - Mobile First Design */

/* More compact .vibe-selector styles */
.vibe-selector {
  display: flex;
  flex-direction: column;
  gap: 0.75rem;
  margin-top: 1rem;
}

.vibe-row {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}

.vibe-card-compact {
  background: rgba(255, 255, 255, 0.9);
  border: 2px solid rgba(46, 134, 171, 0.2);
  border-radius: 8px;
  padding: 0.75rem;
  cursor: pointer;
  transition: all 0.3s ease;
  position: relative;
  flex: 1;
  min-width: 120px;
  max-width: 160px;
  text-align: center;
}

.vibe-card-compact:hover {
  border-color: #2E86AB;
  background: rgba(46, 134, 171, 0.1);
  transform: translateY(-1px);
}

.vibe-card-compact.selected {
  border-color: #2E86AB;
  background: rgba(46, 134, 171, 0.15);
  box-shadow: 0 2px 8px rgba(46, 134, 171, 0.3);
}

.vibe-card-compact.selected::after {
  content: '✓';
  position: absolute;
  top: 0.25rem;
  right: 0.25rem;
  background: #2E86AB;
  color: white;
  width: 18px;
  height: 18px;
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  font-size: 0.7rem;
  font-weight: bold;
}

.vibe-icon-compact {
  font-size: 1.5rem;
  margin-bottom: 0.25rem;
}

.vibe-name-compact {
  font-weight: 600;
  color: #2E86AB;
  font-size: 0.85rem;
  margin: 0;
  line-height: 1.2;
}

/* Mobile: Dropdown selector */
.vibe-dropdown-mobile {
  margin-top: 1rem;
}

.vibe-dropdown-mobile select {
  font-size: 1rem;
}

      .insight-summary {
        background: linear-gradient(135deg, rgba(46, 134, 171, 0.1), rgba(74, 144, 226, 0.1));
        border-left: 4px solid #2E86AB;
        border-radius: 8px;
        padding: 1rem;
        margin-bottom: 1.5rem;
      }

      .insight-summary h5 {
        color: #2E86AB;
        font-weight: 700;
        margin-bottom: 0.5rem;
        display: flex;
        align-items: center;
        gap: 0.5rem;
      }

      .empty-state {
        text-align: center;
        padding: 3rem 1rem;
        color: #6c757d;
      }

      .empty-state .empty-icon {
        font-size: 3rem;
        margin-bottom: 1rem;
        opacity: 0.5;
      }

      .empty-state .empty-title {
        font-size: 1.2rem;
        font-weight: 600;
        margin-bottom: 0.5rem;
        color: #2E86AB;
      }

      .empty-state .empty-subtitle {
        font-size: 0.9rem;
        margin-bottom: 0;
      }

      @media (max-width: 768px) {
        .hero-title {
          font-size: 2rem;
        }

        .search-card {
          padding: 1.5rem;
        }

        .vibe-selector {
          grid-template-columns: 1fr;
        }

        .step-header {
          flex-direction: row;
          align-items: flex-start;
          gap: 0.75rem;
        }

        .step-title {
          font-size: 1.1rem;
        }

        .player-preview {
          flex-direction: column;
          text-align: center;
          gap: 0.75rem;
        }
      }
    ")),
    add_busy_bar(color = "#2E86AB", height = "25px"),

      # Keep-alive and reconnect loop temporarily disabled
    tags$script(HTML(" 
  $(document).ready(function() {
    var lastAnalysisTime = 0;
    var userScrolledUp = false;
    var analysisScrollTimeout;

    // Track user scroll behavior
    $(window).on('scroll', function() {
      var scrollTop = $(window).scrollTop();
      var analysisSection = $('.step-card.active .analysis-content');

      if (analysisSection.length > 0) {
        var analysisTop = analysisSection.offset().top - 100;
        userScrolledUp = scrollTop < analysisTop;
      }
    });

    // Vibe card interaction handler
    $(document).on('click', '.vibe-card', function() {
      var mode = $(this).data('mode');

      // Update visual selection
      $('.vibe-card').removeClass('selected');
      $(this).addClass('selected');

      // Send to Shiny
      Shiny.setInputValue('analysis_mode', mode, {priority: 'event'});

      // Mark that analysis was just triggered
      lastAnalysisTime = Date.now();
      userScrolledUp = false; // Reset scroll tracking

      console.log('🎨 Analysis mode changed to:', mode);
    });

    // Smart scroll to analysis - only when analysis is newly generated
    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        // Look for new analysis content
        if (mutation.type === 'childList') {
          var newAnalysisContent = $(mutation.addedNodes).find('.analysis-content');
          if (newAnalysisContent.length > 0) {
            var timeSinceAnalysis = Date.now() - lastAnalysisTime;

            // Only auto-scroll if:
            // 1. Analysis was recently triggered (within 5 seconds)
            // 2. User hasn't deliberately scrolled up
            if (timeSinceAnalysis < 5000 && !userScrolledUp) {
              clearTimeout(analysisScrollTimeout);
              analysisScrollTimeout = setTimeout(function() {
                newAnalysisContent[0].scrollIntoView({
                  behavior: 'smooth',
                  block: 'start'
                });
                console.log('📜 Auto-scrolled to new analysis');
              }, 800); // Slight delay to let content render
            } else {
              console.log('📜 Skipped auto-scroll (user control)');
            }
          }
        }
      });
    });

    // Start observing
    observer.observe(document.body, {
      childList: true,
      subtree: true
    });

    console.log('✅ Smart scroll interactions initialized');
  });
")),
    tags$script(HTML("\n      Shiny.addCustomMessageHandler('open-x-share', function(msg) {\n        var url = 'https://twitter.com/intent/tweet?text=' + encodeURIComponent(msg.text) + '&url=' + encodeURIComponent(msg.url);\n        window.open(url, '_blank');\n      });\n    "))
  ),
  nav_panel(
    title = "Analysis",
    icon = icon("chart-line"),

    # Hero Section
    div(
      class = "hero-section",
      h1(class = "hero-title", "⚾ McFARLAND ⚾"),
      p(class = "hero-subtitle", "Advanced baseball analysis. Plain English.")
    ),

    # Quick Instructional Alert
    div(
      class = "alert alert-dismissible fade show instruction-alert",
      role = "alert",
      HTML(
        "<div class='instruction-steps'>
            <span>🧢 Pick a player</span>
            <span>&rarr;</span>
            <span>🎧 Choose a vibe</span>
            <span>&rarr;</span>
            <span>📊 Read the analysis</span>
            <span>&rarr;</span>
            <span>🤝 Share with friends</span>
        </div>"
      ),
      tags$button(
        type = "button",
        class = "btn-close",
        `data-bs-dismiss` = "alert",
        `aria-label` = "Close"
      )
    ),

    # Step 1: Player Selection
    div(
      class = "step-card active",
      div(
        class = "step-header",
        div(class = "step-number", "1"),
        h3(class = "step-title", "Choose Players")
      ),
      div(
        class = "mb-3",
        radioButtons(
          "analysis_view",
          label = NULL,
          choices = c("Single Player" = "single", "Compare Players" = "compare"),
          selected = "single",
          inline = TRUE
        )
      ),
      conditionalPanel(
        condition = "input.analysis_view === 'single'",
        div(
          class = "search-input-container",
          selectizeInput(
            inputId = "player_selection",
            label = NULL,
            choices = NULL,
            options = list(
              placeholder = "Type a player name",
              openOnFocus = FALSE,
              closeAfterSelect = TRUE,
              maxOptions = 5,
              onDropdownOpen = I("function(dropdown) { if (!this.lastQuery.length) { this.close(); } }")
            ),
            width = "100%"
          )
        ),
        uiOutput("player_preview")
      ),
      conditionalPanel(
        condition = "input.analysis_view === 'compare'",
        tagList(
          radioButtons(
            "compare_type",
            "Player Type",
            choices = c("Hitters" = "hitter", "Pitchers" = "pitcher"),
            inline = TRUE
          ),
          fluidRow(
            column(
              4,
              selectizeInput(
                "compare_player1",
                "Player 1",
                choices = NULL,
                options = list(placeholder = "Select player"),
                width = "100%"
              )
            ),
            column(
              4,
              selectizeInput(
                "compare_player2",
                "Player 2",
                choices = NULL,
                options = list(placeholder = "Select player"),
                width = "100%"
              )
            ),
            column(
              4,
              selectizeInput(
                "compare_player3",
                "Player 3",
                choices = NULL,
                options = list(placeholder = "Select player"),
                width = "100%"
              )
            )
          )
        )
      )
    ),

    # Step 2: Analysis Style
    uiOutput("step_2_analysis_style"),

    # Step 3: Analysis Results
    uiOutput("step_3_analysis_results")
  ),


  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    
    # Hero Section with proper visual container
    div(
      class = "about-hero-wrapper",
      div(
        class = "about-hero",
        div(
          class = "hero-content",
          div(
            class = "hero-text",
            div(
              class = "hero-text-container",
              h1(class = "about-title", "Meet McFARLAND"),
              h2(class = "about-subtitle", "🤖⚾ Machine-crafted Forecasting And Reasoning for Luck, Analytics, Narratives, and Data"),
              p(class = "about-description", 
                "Your baseball analysis companion that cuts through the noise to tell you what's really happening -- in plain English! -- with MLB players. We separate skill from luck, trends from flukes, and give you insights that matter."
              )
            )
          ),
          div(
            class = "hero-image",
            img(
              src = "tjmcfarland.png", 
              alt = "T.J. McFarland - The inspiration behind our name",
              class = "mcfarland-photo"
            )
          )
        )
      )
    ),
    
    # Main Content Cards
    div(
      class = "about-content",
      
      # App Features Card
      card(
        class = "feature-card",
        card_header(
          icon("chart-line"), "App Features"
        ),
        card_body(
          div(
            class = "feature-grid",
            div(
              class = "feature-item",
              icon("brain", class = "feature-icon"),
              h5("Smart Analysis"),
              p("Insights that explain not just what's happening, but why it matters for the rest of the season.")
            ),
            div(
              class = "feature-item",
              icon("dice", class = "feature-icon"),
              h5("Skill vs Luck"),
              p("We separate sustainable performance changes from statistical noise using advanced metrics like xwOBA and BABIP.")
            ),
            div(
              class = "feature-item",
              icon("users", class = "feature-icon"),
              h5("Both Hitters & Pitchers"),
              p("Comprehensive analysis for position players and pitchers with metrics tailored to each role.")
            ),
            div(
              class = "feature-item",
              icon("palette", class = "feature-icon"),
              h5("Your Style"),
              p("Choose from 9 analysis personalities (Straightforward is the default) - from Shakespeare to Gen Z to Old School wisdom.")
            )
          )
        )
      ),
      
      # Get Connected Card - MOVED UP for better visibility
      card(
        class = "connect-card",
        card_header(
          icon("heart"), "Get Connected"
        ),
        card_body(
          p("McFARLAND is a passion project that gets better with your feedback. Whether you've found a bug, have an idea for a feature, or just want to say hello, we'd love to hear from you!"),
          div(
            class = "connect-buttons",
            tags$a(
              href = "https://docs.google.com/forms/d/e/1FAIpQLScPiHfO2XxwCXd2V-7pNsUKs-mMaqzzsH2ohA_kBflk_n8AQw/viewform",
              target = "_blank", 
              class = "btn btn-primary connect-btn",
              icon("bell"), "Get Updates"
            ),
            tags$a(
              href = "https://forms.gle/NDJJKj7XrsnFH6m16",
              target = "_blank", 
              class = "btn btn-primary connect-btn",
              icon("comment"), "Send Feedback"
            )
          ),
          div(
            class = "connect-note",
            p(
              icon("shield-alt"), 
              tags$strong("Privacy-First:"), 
              " We only collect anonymous usage data to improve the app. No personal information is stored or shared."
            )
          )
        )
      ),
      
      
      # The Story Card
      card(
        class = "story-card",
        card_header(
          icon("lightbulb"), "The Story"
        ),
        card_body(
          p("Named after ", tags$strong("T.J. McFarland"), " - the utility pitcher who embodied versatility and adaptation - our app does what great utility players do: it adapts to what you need."),
          p("Whether you're trying to figure out if your favorite player's hot streak is real, wondering if that pitcher's suddenly great season will continue, or just want to understand baseball better, McFARLAND gives you the insights that matter."),
          p("We built this because baseball analysis shouldn't require a statistics degree. Everyone deserves to understand what's really happening beyond the basic stats.")
        )
      ),
      
      
      
      # What's New Section
      card(
        class = "updates-card",
        card_header(
          icon("sparkles"), "What's New"
        ),
        card_body(
          div(
            class = "version-timeline",
            
            # Latest Version - Highlighted
            div(
              class = "version-item current",
              div(
                class = "version-marker current",
                span(class = "version-dot"),
                span(class = "version-number", "v1.2")
              ),
              div(
                class = "version-content",
                div(
                  class = "version-header",
                  h4("Player Comparison Mode", class = "version-title"),
                  span(class = "version-badge new", "LATEST")
                ),
                p(
                  class = "version-description",
                  "Stack up to three hitters or pitchers at once, get side-by-side stat cards, see our recommendation, and read AI breakdowns tailored to your vibe."
                )
              )
            ),

            # Recent Updates
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v1.1")
              ),
              div(
                class = "version-content",
                h5("Shareable Insights & Deep Links"),
                p(
                  "One-click Share on X builds deep links with your selected players, view, and vibe so friends load the same analysis instantly."
                )
              )
            ),

            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v1.0")
              ),
              div(
                class = "version-content",
                h5("Complete Experience Redesign"),
                p(
                  "We rebuilt the entire app with a guided, step-by-step experience. Now you get instant player insights, smooth interactions, and a much more intuitive flow from player selection to AI analysis."
                )
              )
            ),

            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.9")
              ),
              div(
                class = "version-content",
                h5("Smart Caching & Performance"),
                p("Lightning-fast responses with intelligent caching. Previously analyzed players load instantly.")
              )
            ),
            
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.8")
              ),
              div(
                class = "version-content",
                h5("Player Photos"),
                p("Official MLB headshots for every player. Now you can put a face to the stats.")
              )
            ),
            
            div(
              class = "version-item major",
              div(
                class = "version-marker major",
                span(class = "version-dot"),
                span(class = "version-number", "v0.7")
              ),
              div(
                class = "version-content",
                h5("Pitcher Analysis"),
                p("Major expansion! Added complete pitcher analysis covering all of MLB.")
              )
            ),
            
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.6")
              ),
              div(
                class = "version-content",
                h5("Stay Connected"),
                p("Notification signup and feedback systems to keep you updated.")
              )
            ),
            
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.5")
              ),
              div(
                class = "version-content",
                h5("Advanced Metrics"),
                p("Enhanced analysis with Barrel% and xwOBA to better distinguish skill from luck.")
              )
            ),
            
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.4")
              ),
              div(
                class = "version-content",
                h5("Visual Trends"),
                p("Beautiful charts showing how current season compares to recent history.")
              )
            ),
            
            div(
              class = "version-item beloved",
              div(
                class = "version-marker beloved",
                span(class = "version-dot"),
                span(class = "version-number", "v0.3")
              ),
              div(
                class = "version-content",
                h5("Shakespeare Mode"),
                p("The fan-favorite feature: baseball analysis in iambic pentameter!")
              )
            ),
            
            div(
              class = "version-item",
              div(
                class = "version-marker",
                span(class = "version-dot"),
                span(class = "version-number", "v0.2")
              ),
              div(
                class = "version-content",
                h5("Analysis Personalities"),
                p("Multiple analysis vibes from Analytics Dork to Old Coot.")
              )
            ),
            
            div(
              class = "version-item founding",
              div(
                class = "version-marker founding",
                span(class = "version-dot"),
                span(class = "version-number", "v0.1")
              ),
              div(
                class = "version-content",
                div(
                  class = "version-header",
                  h5("The Beginning", class = "version-title founding"),
                  span(class = "version-badge founding", "FOUNDING")
                ),
                p(class = "version-description founding",
                  "The first version we weren't horrendously ashamed of! Basic hitter analysis comparing 2025 stats to recent history."
                )
              )
            )
          )
        )
      ),
      
    
      
      # Data & Tech Card
      card(
        class = "tech-card",
        card_header(
          icon("database"), "Data & Technology"
        ),
        card_body(
          div(
            class = "tech-grid",
            div(
              class = "tech-item",
              h5("📊 Data Sources"),
              tags$ul(
                tags$li(tags$strong("FanGraphs:"), " Comprehensive baseball statistics and advanced metrics"),
                tags$li(tags$strong("MLB:"), " Official player photos and league data"),
                tags$li(tags$strong("Daily Updates:"), " Fresh statistics refreshed every morning")
              )
            ),
            div(
              class = "tech-item",
              h5("🤖 AI & Analysis"),
              tags$ul(
                tags$li(tags$strong("GPT-4.1:"), " Advanced language model for intelligent analysis"),
                tags$li(tags$strong("Smart Caching:"), " Faster responses and cost optimization"),
                tags$li(tags$strong("Context Aware:"), " Analysis considers age, position, and sample size")
              )
            ),
            div(
              class = "tech-item",
              h5("⚙️ Built With"),
              tags$ul(
                tags$li(tags$strong("R & Shiny:"), " Robust statistical computing and web framework"),
                tags$li(tags$strong("Tidyverse:"), " Clean, maintainable data processing"),
                tags$li(tags$strong("Bootstrap & Custom CSS:"), " Modern, responsive design")
              )
            ),
            div(
              class = "tech-item",
              h5("📈 What We Compare"),
              tags$ul(
                tags$li(tags$strong("2025 Season:"), " Current year performance"),
                tags$li(tags$strong("vs 2022-2024:"), " Three-year historical average"),
                tags$li(tags$strong("500+ Players:"), " Both hitters and pitchers")
              )
            )
          )
        )
      )
    ),
    
    # Footer with proper visual container
    div(
      class = "about-footer-wrapper",
      div(
        class = "about-footer",
        p("Built with ❤️ and ⚾ by Ryan Pollack"),
        p(
          class = "credits",
          "Player data courtesy of ", tags$strong("FanGraphs"), " • Photos courtesy of ", tags$strong("MLB"), 
          " • Powered by ", tags$strong("OpenAI GPT-4.1")
        )
      )
    )
  )
  
  
)

