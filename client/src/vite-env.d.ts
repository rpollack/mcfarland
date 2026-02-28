/// <reference types="vite/client" />

type AmplitudeClient = {
  track: (eventName: string, eventProperties?: Record<string, unknown>) => void;
  setUserId?: (userId: string) => void;
};

interface Window {
  amplitude?: AmplitudeClient;
}

declare module "*.module.css" {
  const classes: Record<string, string>;
  export default classes;
}

declare module "*.module.scss" {
  const classes: Record<string, string>;
  export default classes;
}

declare module "*.module.sass" {
  const classes: Record<string, string>;
  export default classes;
}
