import { Navigate, Route, Routes } from "react-router-dom";
import Layout from "./components/Layout";
import SinglePlayerPage from "./pages/SinglePlayerPage";
import ComparePage from "./pages/ComparePage";
import AboutPage from "./pages/AboutPage";
import { VibeProvider } from "./contexts/VibeContext";

function App() {
  return (
    <VibeProvider>
      <Layout>
        <Routes>
          <Route path="/" element={<SinglePlayerPage />} />
          <Route path="/compare" element={<ComparePage />} />
          <Route path="/about" element={<AboutPage />} />
          <Route path="*" element={<Navigate to="/" replace />} />
        </Routes>
      </Layout>
    </VibeProvider>
  );
}

export default App;
