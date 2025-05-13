import React, { useState, useEffect } from 'react';
import { mnesiaService, UserSession } from '../lib/mnesia';
import { Badge } from '@/components/ui/badge';
import { websocketService } from '../lib/websocketService';

const UserStatus: React.FC = () => {
  const [session, setSession] = useState<UserSession>({ pseudo: null, loggedIn: false });
  const [connectionStatus, setConnectionStatus] = useState<'connected' | 'disconnected'>('disconnected');

  useEffect(() => {
    // Subscribe to session updates
    const unsubscribe = mnesiaService.subscribeToSession(setSession);
    
    // Check WebSocket connection status
    const checkConnection = () => {
      setConnectionStatus(websocketService.isConnected() ? 'connected' : 'disconnected');
    };
    
    // Set up interval to check connection status
    const interval = setInterval(checkConnection, 2000);
    checkConnection(); // Check immediately
    
    return () => {
      unsubscribe();
      clearInterval(interval);
    };
  }, []);

  return (
    <div className="flex items-center space-x-2 bg-ghost-dark p-2 rounded-lg">
      <div className="flex items-center">
        <span className={`w-2 h-2 rounded-full ${connectionStatus === 'connected' ? 'bg-green-500' : 'bg-red-500'} mr-1`}></span>
        <span className="text-sm text-blue-500">Live Mnesia</span>
      </div>

      {/* Badge avec texte qui change en fonction du statut de la connexion */}
      <Badge 
        variant="outline" 
        className={`text-sm ${connectionStatus === 'connected' ? 'bg-green-500 text-white' : 'bg-red-500 text-ghost-text/70'}`}
      >
        {connectionStatus === 'connected' ? 'Connected' : 'Not connected'}
      </Badge>

      {/* Affichage du badge avec pseudo si l'utilisateur est connecté */}
      {session.loggedIn && connectionStatus === 'connected' && (
        <Badge variant="outline" className="bg-ghost-primary text-ghost-text border border-gray-300 rounded-md">
          Viewing as {session.pseudo}
        </Badge>
      )}
    </div>
  );
};

export default UserStatus;
