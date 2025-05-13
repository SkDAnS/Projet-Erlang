import { defineConfig } from "vite";
import react from "@vitejs/plugin-react-swc";
import path from "path";
import { componentTagger } from "lovable-tagger";

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => ({
  server: {
    host: "::",      // Permet de rendre l'application accessible sur d'autres appareils dans le réseau local
    port: 8081,      // Port sur lequel le serveur de développement va tourner
    hmr: {           // Configuration du Hot Module Replacement
      protocol: "ws", // Utilise WebSocket pour HMR, si tu as un problème de WebSocket, cette option peut être utile
      host: "localhost", // S'assure que HMR se connecte à localhost, même sur un réseau local
    },
  },
  plugins: [
    react(),
    mode === 'development' && componentTagger(), // Utilise le plugin `lovable-tagger` uniquement en mode développement
  ].filter(Boolean), // Filtre les plugins conditionnels
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"), // Alias pour faciliter l'importation des fichiers depuis le dossier `src`
    },
  },
}));
