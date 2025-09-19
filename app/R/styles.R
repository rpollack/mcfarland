# UI Styling ----------------------------------------------------------------

ui_styles <- HTML("
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');

  /* Global app styling with viewport lock */
  * {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  }

  html, body {
    margin: 0;
    padding: 0;
    width: 100%;
    height: 100%;
    overflow-x: hidden !important;
    position: fixed !important;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  }

  #shiny-ui {
    width: 100vw !important;
    height: 100vh !important;
    overflow-x: hidden !important;
    overflow-y: auto !important;
  }

  html.ios-safari #shiny-ui {
    -webkit-overflow-scrolling: touch;
  }

  .ios-safari .navbar,
  .ios-safari .card,
  .ios-safari .player-info-card,
  .ios-safari .compare-card,
  .ios-safari .comparison-card,
  .ios-safari .cache-status,
  .ios-safari .alert,
  .ios-safari .modal-content {
    backdrop-filter: none !important;
    -webkit-backdrop-filter: none !important;
    background: rgba(255, 255, 255, 0.94) !important;
    box-shadow: 0 12px 24px rgba(0, 0, 0, 0.08) !important;
  }

  /* Navbar styling with responsive brand */
  .navbar {
    background: rgba(255, 255, 255, 0.95) !important;
    backdrop-filter: blur(20px);
    border-bottom: 1px solid rgba(255, 255, 255, 0.2);
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.1);
    padding: 0.5rem 1rem !important;
  }

  .navbar-brand {
    font-weight: 700 !important;
    font-size: 1.3rem !important;
    color: #2E86AB !important;
    text-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
    padding: 0.25rem 0 !important;
  }

  .navbar-subtitle {
    font-size: 0.65rem !important;
    color: #6c757d !important;
    font-weight: 400 !important;
    line-height: 1 !important;
    margin-top: 2px !important;
  }

  .nav-link {
    font-weight: 500 !important;
    transition: all 0.3s ease !important;
    border-radius: 8px !important;
    margin: 0 2px !important;
    padding: 0.5rem 0.75rem !important;
    font-size: 0.9rem !important;
  }

  .nav-link:hover {
    background: rgba(46, 134, 171, 0.1) !important;
    transform: translateY(-1px);
  }

  /* Container adjustments for mobile */
  .container-fluid {
    padding-left: 0.75rem !important;
    padding-right: 0.75rem !important;
    max-width: 100% !important;
    overflow-x: hidden !important;
  }

  /* Card styling with glass morphism */
  .card {
    background: rgba(255, 255, 255, 0.95) !important;
    backdrop-filter: blur(20px) !important;
    border: 1px solid rgba(255, 255, 255, 0.2) !important;
    border-radius: 15px !important;
    box-shadow: 0 15px 35px rgba(0, 0, 0, 0.1) !important;
    transition: all 0.3s ease !important;
    overflow: hidden;
    margin-bottom: 1rem !important;
  }

  .card:hover {
    transform: translateY(-3px);
    box-shadow: 0 20px 40px rgba(0, 0, 0, 0.15) !important;
  }

  .card-header {
    background: linear-gradient(135deg, #2E86AB, #4A90E2) !important;
    color: white !important;
    font-weight: 600 !important;
    font-size: 1rem !important;
    border-bottom: none !important;
    padding: 1rem 1.25rem !important;
    text-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
  }

  .card-body {
    padding: 1.25rem !important;
  }

  /* Form controls styling */
  .form-select, .form-control {
    border: 2px solid rgba(46, 134, 171, 0.2) !important;
    border-radius: 10px !important;
    padding: 0.6rem 0.85rem !important;
    font-weight: 500 !important;
    font-size: 0.9rem !important;
    transition: all 0.3s ease !important;
    background: rgba(255, 255, 255, 0.9) !important;
  }

  .form-select:focus, .form-control:focus {
    border-color: #2E86AB !important;
    box-shadow: 0 0 0 3px rgba(46, 134, 171, 0.2) !important;
    transform: scale(1.01);
  }

  /* Button styling */
  .btn {
    border-radius: 10px !important;
    font-weight: 600 !important;
    padding: 0.6rem 1.2rem !important;
    transition: all 0.3s ease !important;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    font-size: 0.8rem !important;
  }

  .btn-primary {
    background: linear-gradient(135deg, #2E86AB, #4A90E2) !important;
    border: none !important;
    box-shadow: 0 4px 15px rgba(46, 134, 171, 0.3) !important;
  }

  .btn-primary:hover {
    transform: translateY(-2px);
    box-shadow: 0 8px 25px rgba(46, 134, 171, 0.4) !important;
  }

  /* CACHE: Cache status styling */
  .cache-status {
    background: rgba(46, 134, 171, 0.1) !important;
    border: 1px solid rgba(46, 134, 171, 0.2) !important;
    border-radius: 8px !important;
    padding: 8px 12px !important;
    font-size: 0.8rem !important;
    color: #2E86AB !important;
  }

  .btn-outline-secondary {
    border-color: rgba(108, 117, 125, 0.3) !important;
    color: #6c757d !important;
  }

  .btn-outline-secondary:hover {
    background: rgba(108, 117, 125, 0.1) !important;
    border-color: #6c757d !important;
  }

  /* Player info card styling */
  .player-info-card {
    background: linear-gradient(135deg, rgba(255, 255, 255, 0.95), rgba(248, 249, 250, 0.95)) !important;
    backdrop-filter: blur(20px) !important;
    border: 1px solid rgba(255, 255, 255, 0.3) !important;
    border-radius: 15px !important;
    padding: 1.5rem !important;
    box-shadow: 0 10px 25px rgba(0, 0, 0, 0.1) !important;
    transition: all 0.4s ease !important;
    margin-bottom: 1rem !important;
  }

  .player-info-card:hover {
    transform: translateY(-2px) scale(1.01);
    box-shadow: 0 15px 35px rgba(0, 0, 0, 0.15) !important;
  }

  .player-photo {
    width: 120px !important;
    height: 120px !important;
    border-radius: 50% !important;
    border: 3px solid #2E86AB !important;
    margin-bottom: 0.75rem !important;
    object-fit: cover !important;
    transition: all 0.3s ease !important;
    box-shadow: 0 8px 20px rgba(46, 134, 171, 0.3) !important;
  }

  .compare-photo {
    width: 80px !important;
    height: 80px !important;
    border-radius: 50% !important;
    border: 3px solid #2E86AB !important;
    object-fit: cover !important;
    margin-bottom: 0.5rem !important;
    box-shadow: 0 6px 15px rgba(46, 134, 171, 0.25) !important;
  }

  .player-photo:hover {
    transform: scale(1.03);
    box-shadow: 0 10px 25px rgba(46, 134, 171, 0.4) !important;
  }

  .player-name {
    margin-bottom: 0.4rem !important;
    color: #2E86AB !important;
    font-weight: 700 !important;
    font-size: 1.2rem !important;
    text-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
  }

  .player-details {
    color: #6c757d !important;
    font-size: 0.85rem !important;
    font-weight: 500 !important;
    margin-bottom: 0 !important;
  }

  /* Alert styling */
  .alert {
    border-radius: 12px !important;
    border: none !important;
    font-weight: 500 !important;
    backdrop-filter: blur(10px) !important;
    font-size: 0.9rem !important;
  }

  .alert-info {
    background: rgba(13, 202, 240, 0.1) !important;
    color: #0c5aa6 !important;
  }

  .alert-warning {
    background: rgba(255, 193, 7, 0.1) !important;
    color: #997404 !important;
  }

  /* Loading animation */
  .progress {
    height: 6px !important;
    border-radius: 10px !important;
    background: rgba(255, 255, 255, 0.2) !important;
  }

  .progress-bar {
    background: linear-gradient(90deg, #2E86AB, #4A90E2) !important;
    border-radius: 10px !important;
  }

  /* Plot container styling */
  .shiny-plot-output {
    border-radius: 12px !important;
    overflow: hidden !important;
    box-shadow: 0 8px 20px rgba(0, 0, 0, 0.1) !important;
  }

  /* Analysis content styling */
  .analysis-content {
    line-height: 1.5 !important;
    font-size: 0.95rem !important;
    color: #212529 !important;
  }

  .analysis-content h1, .analysis-content h2, .analysis-content h3 {
    color: #212529 !important;
    font-weight: 700 !important;
    margin-top: 1.2rem !important;
    margin-bottom: 0.8rem !important;
    font-size: 1.1rem !important;
  }

  .analysis-content p {
    margin-bottom: 0.8rem !important;
    color: #212529 !important;
  }

  /* Mobile-specific optimizations */
  @media (max-width: 768px) {
    .navbar-brand {
      font-size: 1.1rem !important;
      /* Show mobile-specific layout */
.vibe-selector {
  display: none;
}

.vibe-selector-mobile {
  display: flex;
}
    }

    .navbar-subtitle {
      font-size: 0.6rem !important;
    }

    .nav-link {
      font-size: 0.85rem !important;
      padding: 0.4rem 0.6rem !important;
    }

    .card {
      border-radius: 12px !important;
      margin-bottom: 0.75rem !important;
    }

    .card-body {
      padding: 1rem !important;
    }

    .player-info-card {
      padding: 1.2rem !important;
    }

    .player-photo {
      width: 100px !important;
      height: 100px !important;
    }

    .compare-photo {
      width: 60px !important;
      height: 60px !important;
    }

    .player-name {
      font-size: 1.1rem !important;
    }

    .player-details {
      font-size: 0.8rem !important;
    }

    .analysis-content {
      font-size: 0.9rem !important;
      color: #212529 !important;
    }

    .analysis-content h1, .analysis-content h2, .analysis-content h3 {
      font-size: 1rem !important;
      color: #212529 !important;
    }

    /* Ensure columns stack properly on mobile */
    .layout-columns {
      flex-direction: column !important;
    }

    .layout-columns > * {
      width: 100% !important;
      flex: none !important;
    }

    .btn {
      font-size: 0.75rem !important;
      padding: 0.5rem 1rem !important;
    }
  }

  /* AI Status Badge Styling */
.badge {
  font-size: 0.75rem !important;
  padding: 0.35rem 0.65rem !important;
  border-radius: 12px !important;
}

.badge .fa-spinner {
  animation: fa-spin 1s infinite linear;
}

.badge .fa-check-circle {
  color: white !important;
}

/* Status Alert Improvements */
.alert .spinner-border-sm {
  width: 1rem;
  height: 1rem;
  border-width: 0.1em;
}

.btn-outline-success:hover {
  transform: translateY(-1px);
  box-shadow: 0 2px 8px rgba(40, 167, 69, 0.3);
}

/* Mobile responsiveness */
@media (max-width: 768px) {
  .step-header {
    flex-direction: column;
    align-items: flex-start;
    gap: 0.5rem;
  }

  .step-header .badge {
    align-self: flex-start;
    margin-left: 0 !important;
    margin-top: 0.25rem;
  }
}

  /* Extra small screens */
  @media (max-width: 576px) {
    .container-fluid {
      padding-left: 0.5rem !important;
      padding-right: 0.5rem !important;
    }

    .navbar {
      padding: 0.4rem 0.75rem !important;
    }

    .navbar-brand {
      font-size: 1rem !important;
    }

    .navbar-subtitle {
      font-size: 0.55rem !important;
    }

    .card-body {
      padding: 0.85rem !important;
    }

    .player-info-card {
      padding: 1rem !important;
    }

    .form-select, .form-control {
      font-size: 0.85rem !important;
      padding: 0.5rem 0.7rem !important;
    }

    .btn {
      font-size: 0.7rem !important;
      padding: 0.4rem 0.8rem !important;
    }
  }

  /* Smooth scrolling */
  html {
    scroll-behavior: smooth;
  }

  /* Custom scrollbar */
  ::-webkit-scrollbar {
    width: 6px;
  }

  ::-webkit-scrollbar-track {
    background: rgba(255, 255, 255, 0.1);
  }

  ::-webkit-scrollbar-thumb {
    background: rgba(46, 134, 171, 0.5);
    border-radius: 10px;
  }

  ::-webkit-scrollbar-thumb:hover {
    background: rgba(46, 134, 171, 0.7);
  }
  
  /* About Page Styling - Add this to your existing ui_styles HTML block */

/* About Page Hero - Enhanced with proper visual container */
.about-hero-wrapper {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  padding: 2rem 1rem;
  margin: -1rem -1rem 2rem -1rem; /* Extend to edges */
}

.about-hero {
  background: rgba(255, 255, 255, 0.95);
  backdrop-filter: blur(20px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 20px;
  padding: 3rem 2rem;
  box-shadow: 0 20px 40px rgba(0, 0, 0, 0.15);
  max-width: 1200px;
  margin: 0 auto;
}

.hero-content {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 3rem;
}

.hero-text {
  flex: 2;
  text-align: left;
}

.hero-text-container {
  background: rgba(255, 255, 255, 0.8);
  border-radius: 15px;
  padding: 2rem;
  box-shadow: 0 10px 25px rgba(0, 0, 0, 0.1);
  border: 1px solid rgba(255, 255, 255, 0.5);
}

.hero-image {
  flex: 1;
  text-align: center;
}

.about-title {
  font-size: 2.5rem;
  font-weight: 700;
  color: #2E86AB;
  margin-bottom: 0.5rem;
  text-shadow: 0 2px 4px rgba(0,0,0,0.1);
}

.about-subtitle {
  font-size: 1.2rem;
  color: #4A90E2;
  margin-bottom: 1rem;
  font-weight: 500;
}

.about-description {
  font-size: 1.1rem;
  color: #495057;
  line-height: 1.6;
  margin-bottom: 0;
}

.mcfarland-photo {
  max-width: 200px;
  width: 100%;
  height: auto;
  border-radius: 15px;
  box-shadow: 0 10px 25px rgba(0,0,0,0.15);
  border: 3px solid #2E86AB;
}

.about-content {
  display: flex;
  flex-direction: column;
  gap: 2rem;
}

/* Feature Cards */
.feature-card .card-header {
  background: linear-gradient(135deg, #2E86AB, #4A90E2);
}

.feature-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 1.5rem;
}

.feature-item {
  text-align: center;
  padding: 1rem;
  background: rgba(46, 134, 171, 0.05);
  border-radius: 12px;
  transition: transform 0.3s ease;
}

.feature-item:hover {
  transform: translateY(-3px);
  box-shadow: 0 5px 15px rgba(0,0,0,0.1);
}

.feature-icon {
  font-size: 2rem;
  color: #2E86AB;
  margin-bottom: 0.75rem;
}

.feature-item h5 {
  color: #2E86AB;
  font-weight: 600;
  margin-bottom: 0.5rem;
}

.feature-item p {
  color: #6c757d;
  font-size: 0.9rem;
  line-height: 1.4;
  margin-bottom: 0;
}

/* Story Card */
.story-card .card-header {
  background: linear-gradient(135deg, #28a745, #20c997);
}

.story-card p {
  font-size: 1rem;
  line-height: 1.6;
  color: #495057;
}

/* Updates Card */
.updates-card .card-header {
  background: linear-gradient(135deg, #ffc107, #fd7e14);
}

/* Version Timeline */
.version-timeline {
  position: relative;
  padding-left: 0;
}

.version-item {
  display: flex;
  align-items: flex-start;
  margin-bottom: 1.5rem;
  position: relative;
}

.version-item:not(:last-child):before {
  content: '';
  position: absolute;
  left: 22px;
  top: 45px;
  bottom: -24px;
  width: 2px;
  background: #dee2e6;
}

.version-item.current:not(:last-child):before {
  background: #2E86AB;
}

.version-marker {
  display: flex;
  flex-direction: column;
  align-items: center;
  margin-right: 1rem;
  position: relative;
  z-index: 2;
}

.version-dot {
  width: 12px;
  height: 12px;
  border-radius: 50%;
  background: #dee2e6;
  border: 3px solid white;
  box-shadow: 0 0 0 2px #dee2e6;
}

.version-item.current .version-dot {
  background: #2E86AB;
  box-shadow: 0 0 0 2px #2E86AB;
}

.version-item.major .version-dot {
  background: #dc3545;
  box-shadow: 0 0 0 2px #dc3545;
}

.version-item.beloved .version-dot {
  background: #6f42c1;
  box-shadow: 0 0 0 2px #6f42c1;
}

.version-item.founding .version-dot {
  background: #ffc107;
  box-shadow: 0 0 0 2px #ffc107;
}

.version-number {
  font-size: 0.7rem;
  font-weight: 600;
  color: #6c757d;
  margin-top: 0.25rem;
  text-align: center;
}

.version-content {
  flex: 1;
  min-width: 0;
}

.version-header {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  margin-bottom: 0.5rem;
  flex-wrap: wrap;
}

.version-title {
  font-weight: 600;
  color: #2E86AB;
  margin: 0;
  font-size: 1.1rem;
}

.version-title.founding {
  color: #ffc107;
}

.version-badge {
  font-size: 0.7rem;
  font-weight: 600;
  padding: 0.2rem 0.5rem;
  border-radius: 10px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.version-badge.new {
  background: #28a745;
  color: white;
}

.version-badge.founding {
  background: #ffc107;
  color: #212529;
}

.version-date {
  font-size: 0.8rem;
  color: #6c757d;
  margin-left: auto;
}

.version-description {
  color: #495057;
  line-height: 1.4;
  font-size: 0.9rem;
  margin: 0;
}

.version-description.founding {
  font-style: italic;
}

.version-content h5 {
  color: #2E86AB;
  font-weight: 600;
  margin-bottom: 0.5rem;
  font-size: 1rem;
}

.version-content p {
  color: #6c757d;
  font-size: 0.9rem;
  line-height: 1.4;
  margin: 0;
}

/* Tech Card */
.tech-card .card-header {
  background: linear-gradient(135deg, #6f42c1, #e83e8c);
}

.tech-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
  gap: 1.5rem;
}

.tech-item h5 {
  color: #2E86AB;
  font-weight: 600;
  margin-bottom: 0.75rem;
  font-size: 1rem;
}

.tech-item ul {
  margin: 0;
  padding-left: 1.2rem;
}

.tech-item li {
  color: #495057;
  font-size: 0.9rem;
  line-height: 1.5;
  margin-bottom: 0.5rem;
}

.tech-item strong {
  color: #2E86AB;
}

/* Connect Card */
.connect-card .card-header {
  background: linear-gradient(135deg, #dc3545, #fd7e14);
}

.connect-buttons {
  display: flex;
  gap: 1rem;
  margin: 1.5rem 0;
}

.connect-btn {
  flex: 1;
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 0.5rem;
  padding: 0.75rem 1rem;
  font-weight: 600;
  text-decoration: none;
}

.connect-btn:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 15px rgba(46, 134, 171, 0.3);
}

.connect-note {
  background: rgba(46, 134, 171, 0.05);
  border: 1px solid rgba(46, 134, 171, 0.2);
  border-radius: 8px;
  padding: 1rem;
  margin-top: 1rem;
}

.connect-note p {
  margin: 0;
  font-size: 0.9rem;
  color: #495057;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.connect-note .fa-shield-alt {
  color: #28a745;
}

/* About Footer - Enhanced with proper visual container */
.about-footer-wrapper {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  padding: 1.5rem 1rem;
  margin: 2rem -1rem -1rem -1rem; /* Extend to edges, connect to bottom */
}

.about-footer {
  background: rgba(255, 255, 255, 0.95);
  backdrop-filter: blur(20px);
  border: 1px solid rgba(255, 255, 255, 0.2);
  border-radius: 15px;
  padding: 2rem;
  box-shadow: 0 15px 35px rgba(0, 0, 0, 0.15);
  max-width: 1200px;
  margin: 0 auto;
  text-align: center;
  border-top: none; /* Remove the old border */
}

.about-footer p {
  margin: 0.5rem 0;
  color: #495057; /* Darker text for better contrast */
}

.credits {
  font-size: 0.9rem;
}

.credits strong {
  color: #2E86AB;
}

/* About Page Mobile Responsiveness */
@media (max-width: 768px) {
  .about-hero-wrapper {
    padding: 1.5rem 0.5rem;
    margin: -0.75rem -0.75rem 1.5rem -0.75rem;
  }
  
  .about-hero {
    padding: 2rem 1.5rem;
  }
  
  .about-footer-wrapper {
    padding: 1.25rem 0.5rem;
    margin: 2rem -0.75rem -0.75rem -0.75rem;
  }
  
  .about-footer {
    padding: 1.5rem;
  }
  
  .hero-content {
    flex-direction: column;
    text-align: center;
    gap: 2rem;
  }
  
  .hero-text {
    text-align: center;
  }
  
  .hero-text-container {
    padding: 1.5rem;
  }
  
  .about-title {
    font-size: 2rem;
  }
  
  .about-subtitle {
    font-size: 1rem;
  }
  
  .about-description {
    font-size: 1rem;
  }
  
  .mcfarland-photo {
    max-width: 150px;
  }
  
  .feature-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
  
  .tech-grid {
    grid-template-columns: 1fr;
    gap: 1rem;
  }
  
  .connect-buttons {
    flex-direction: column;
  }
  
  .version-header {
    flex-direction: column;
    align-items: flex-start;
    gap: 0.5rem;
  }
  
  .version-date {
    margin-left: 0;
    align-self: flex-start;
  }
  
  .version-timeline {
    padding-left: 0;
  }
  
  .version-item {
    margin-bottom: 2rem;
  }
}

@media (max-width: 576px) {
  .about-hero-wrapper {
    padding: 1rem 0.25rem;
    margin: -0.75rem -0.75rem 1rem -0.75rem;
  }
  
  .about-hero {
    padding: 1.5rem 1rem;
  }
  
  .about-footer-wrapper {
    padding: 1rem 0.25rem;
    margin: 1.5rem -0.75rem -0.75rem -0.75rem;
  }
  
  .about-footer {
    padding: 1.25rem;
  }
  
  .hero-text-container {
    padding: 1.25rem;
  }
  
  .about-title {
    font-size: 1.75rem;
  }
  
  .about-subtitle {
    font-size: 0.9rem;
  }
  
  .feature-item {
    padding: 0.75rem;
  }
  
  .version-description {
    font-size: 0.85rem;
  }
}

/* Player Stat Line Styling */
  .player-stat-line {
    background: rgba(46, 134, 171, 0.05);
    border: 1px solid rgba(46, 134, 171, 0.15);
    border-radius: 12px;
    padding: 1rem;
    margin: 1rem 0;
  }

  .stat-line-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 0.75rem;
    padding-bottom: 0.5rem;
    border-bottom: 1px solid rgba(46, 134, 171, 0.2);
  }

  .stat-line-title {
    font-weight: 600;
    color: #2E86AB;
    font-size: 0.95rem;
    margin: 0;
  }

  .stat-line-context {
    font-size: 0.8rem;
    color: #6c757d;
    font-weight: 500;
  }

  .player-preview-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(80px, 1fr));
    gap: 0.75rem;
    width: 100%;
    align-items: center;
  }

  .player-preview-avatar {
    width: 80px;
    height: 80px;
    border-radius: 50%;
    object-fit: cover;
  }

  .player-preview-info {
    grid-column: span 2;
    display: flex;
    flex-direction: column;
    justify-content: center;
  }

  .stats-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(80px, 1fr));
    gap: 0.75rem;
    width: 100%;
  }

  .stat-item {
    text-align: center;
    padding: 0.5rem;
    background: rgba(255, 255, 255, 0.7);
    border-radius: 8px;
    border: 1px solid rgba(255, 255, 255, 0.5);
    transition: all 0.3s ease;
    position: relative;
  }

  .stat-item:hover {
    transform: translateY(-1px);
    box-shadow: 0 2px 8px rgba(46, 134, 171, 0.15);
  }

  .stat-label {
    font-size: 0.75rem;
    font-weight: 600;
    color: #6c757d;
    margin-bottom: 0.25rem;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .stat-value {
    font-size: 1rem;
    font-weight: 700;
    color: #2E86AB;
    margin: 0;
  }

  /* Mobile responsiveness */
  @media (max-width: 768px) {
    .stats-grid {
      grid-template-columns: repeat(3, 1fr);
      gap: 0.5rem;
    }
    
    .stat-item {
      padding: 0.4rem;
    }
    
    .stat-value {
      font-size: 0.9rem;
    }
  }

  @media (max-width: 576px) {
    .stats-grid {
      grid-template-columns: repeat(2, 1fr);
    }
    
    .stat-line-header {
      flex-direction: column;
      align-items: flex-start;
      gap: 0.25rem;
    }
  }
  
")

