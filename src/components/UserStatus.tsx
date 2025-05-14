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
  <div className="host-dark p-2 rounded-lg">
    <div className="border-2 border-black p-2 inline-flex items-center gap-2 font-mono">
      <span className={`w-2 h-2 rounded-full ${connectionStatus === 'connected' ? 'bg-green-500' : 'bg-red-500'} mr-1`}></span>
      <span className="text-sm text-black">Live Mnesia</span>
      {/* Texte et badge du statut de connexion dans le même rectangle */}
      <span 
        className={`text-sm ml-2 ${connectionStatus === 'connected' ? 'text-green-500' : 'text-red-500'}`}
      >
        {connectionStatus === 'connected' ? 'Connected' : 'Not connected'}
      </span>
    

    {/* Affichage du badge avec pseudo si l'utilisateur est connecté */}
    {session.loggedIn && connectionStatus === 'connected' && (
      <Badge variant="outline" className="bg-ghost-dark text-ghost-text/70">
        Affichage en tant que {session.pseudo}
      </Badge>
    )}
    </div>
  </div>
);

};

export default UserStatus;
