
// WebSocket service pour la communication avec le backend Erlang
class WebSocketService {
  private socket: WebSocket | null = null;
  private reconnectTimeout: number | null = null;
  private listeners: Map<string, ((data: any) => void)[]> = new Map();
  private connected = false;
  private url: string;

  constructor(url: string) {
    this.url = url;
  }

  connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      if (this.socket && this.connected) {
        resolve();
        return;
      }

      try {
        console.log(`Connecting to WebSocket at ${this.url}`);
        this.socket = new WebSocket(this.url);

        this.socket.onopen = () => {
          console.log("WebSocket connection established");
          this.connected = true;
          if (this.reconnectTimeout) {
            clearTimeout(this.reconnectTimeout);
            this.reconnectTimeout = null;
          }
          resolve();
        };

        this.socket.onclose = (event) => {
          console.log(`WebSocket connection closed: ${event.code} ${event.reason}`);
          this.connected = false;
          this.scheduleReconnect();
        };

        this.socket.onerror = (error) => {
          console.error("WebSocket error:", error);
          if (!this.connected) {
            reject(error);
          }
        };

        this.socket.onmessage = (event) => {
          try {
            const message = JSON.parse(event.data);
            const { type, data } = message;
            
            if (type && this.listeners.has(type)) {
              const typeListeners = this.listeners.get(type) || [];
              typeListeners.forEach(callback => callback(data));
            }
          } catch (error) {
            console.error("Error parsing WebSocket message:", error);
          }
        };
      } catch (error) {
        console.error("Error creating WebSocket:", error);
        this.scheduleReconnect();
        reject(error);
      }
    });
  }

  private scheduleReconnect() {
    if (!this.reconnectTimeout) {
      this.reconnectTimeout = window.setTimeout(() => {
        console.log("Attempting to reconnect...");
        this.connect().catch(() => {
          // Silent catch as the error is already logged in connect
        });
      }, 5000);
    }
  }

  disconnect() {
    if (this.socket) {
      this.socket.close();
      this.socket = null;
      this.connected = false;
    }
    if (this.reconnectTimeout) {
      clearTimeout(this.reconnectTimeout);
      this.reconnectTimeout = null;
    }
  }

  send(type: string, data?: any) {
    if (!this.socket || !this.connected) {
      console.error("Cannot send message: WebSocket not connected");
      return false;
    }

    const message = JSON.stringify({ type, data });
    this.socket.send(message);
    return true;
  }

  subscribe(type: string, callback: (data: any) => void) {
    if (!this.listeners.has(type)) {
      this.listeners.set(type, []);
    }
    const typeListeners = this.listeners.get(type) || [];
    typeListeners.push(callback);
    this.listeners.set(type, typeListeners);

    return () => {
      const updatedListeners = (this.listeners.get(type) || []).filter(cb => cb !== callback);
      this.listeners.set(type, updatedListeners);
    };
  }

  isConnected(): boolean {
    return this.connected;
  }
}

// Exportez une instance du service avec une URL par défaut
// Cette URL doit être remplacée par l'URL réelle de votre serveur Erlang WebSocket
export const websocketService = new WebSocketService("ws://localhost:8080/websocket");

