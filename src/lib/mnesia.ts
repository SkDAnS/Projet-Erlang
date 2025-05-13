
import { websocketService } from './websocketService';

// Interface definitions for Mnesia data structures
export interface Reply {
  id: number;
  pseudo: string;
  text: string;
}

export interface Message {
  id: number;
  pseudo: string;
  text: string;
  likes: number;
  replies: Reply[];
}

export interface UserSession {
  pseudo: string | null;
  loggedIn: boolean;
}

// Service that connects to Erlang/Mnesia via WebSocket
class MnesiaService {
  private messages: Message[] = [];
  private session: UserSession = { pseudo: null, loggedIn: false };
  private listeners: ((messages: Message[]) => void)[] = [];
  private sessionListeners: ((session: UserSession) => void)[] = [];
  private initialized = false;

  constructor() {
    console.log("Initializing Mnesia service with WebSocket connection...");
  }

  async initialize() {
    if (this.initialized) return;
    
    try {
      await websocketService.connect();
      this.initialized = true;
      
      // Subscribe to messages updates from the server
      websocketService.subscribe('messages', (data: Message[]) => {
        this.messages = data;
        this.notifyListeners();
      });

      // Subscribe to session updates from the server
      websocketService.subscribe('session', (data: UserSession) => {
        this.session = data;
        this.notifySessionListeners();
      });

      // Request initial data
      this.requestMessages();
      this.requestSessionInfo();

      console.log("Mnesia service initialized");
    } catch (error) {
      console.error("Failed to initialize Mnesia service:", error);
    }
  }

  // Request messages from the server
  requestMessages() {
    websocketService.send('get_messages');
  }

  // Request session info from the server
  requestSessionInfo() {
    websocketService.send('get_session');
  }

  // Start monitoring for updates
  startPolling() {
    this.initialize();
    // We don't need to poll with WebSockets as updates are push-based
  }

  stopPolling() {
    // We don't need to poll with WebSockets
  }

  // Subscribe to message updates
  subscribeToMessages(callback: (messages: Message[]) => void) {
    this.initialize();
    this.listeners.push(callback);
    
    // If we already have messages, immediately call with current data
    if (this.messages.length > 0) {
      callback(this.messages);
    }
    
    return () => {
      this.listeners = this.listeners.filter(cb => cb !== callback);
    };
  }

  // Subscribe to session updates
  subscribeToSession(callback: (session: UserSession) => void) {
    this.initialize();
    this.sessionListeners.push(callback);
    
    // Immediately call with current data
    callback(this.session);
    
    return () => {
      this.sessionListeners = this.sessionListeners.filter(cb => cb !== callback);
    };
  }

  // Notify all listeners about message updates
  private notifyListeners() {
    this.listeners.forEach(callback => callback(this.messages));
  }

  // Notify all session listeners
  private notifySessionListeners() {
    this.sessionListeners.forEach(callback => callback(this.session));
  }

  // Get all messages
  getMessages(): Message[] {
    return this.messages;
  }

  // Get current session
  getSession(): UserSession {
    return this.session;
  }

  // Login user
  async login(pseudo: string, password: string): Promise<void> {
    return new Promise((resolve, reject) => {
      const responseHandler = (response: any) => {
        if (response.success) {
          resolve();
        } else {
          reject(new Error(response.error || 'Login failed'));
        }
      };

      const unsubscribe = websocketService.subscribe('login_response', responseHandler);
      websocketService.send('login', { pseudo, password });

      // Remove the subscription after a timeout
      setTimeout(() => {
        unsubscribe();
        reject(new Error('Login request timed out'));
      }, 5000);
    });
  }

  // Logout user
  logout() {
    websocketService.send('logout');
  }
}

// Export a singleton instance
export const mnesiaService = new MnesiaService();
