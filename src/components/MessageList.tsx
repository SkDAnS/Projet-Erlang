import React, { useState, useEffect } from 'react';
import { Message as MessageType, mnesiaService } from '../lib/mnesia';
import Message from './Message';
import { toast } from "sonner";

const MessageList: React.FC = () => {
  const [messages, setMessages] = useState<MessageType[]>([]);
  const [loading, setLoading] = useState<boolean>(true);

  useEffect(() => {
    // Subscribe to message updates
    const unsubscribe = mnesiaService.subscribeToMessages((newMessages) => {
      setMessages(newMessages);
      setLoading(false);
    });
    
    // Start polling for updates
    mnesiaService.startPolling();
    
    // Show connection toast
    toast.info("Connexion à la base de données Mnesia...");
    
    // Clean up subscription and polling on unmount
    return () => {
      unsubscribe();
      mnesiaService.stopPolling();
    };
  }, []);

  return (
    <div className="border-2 border-black p-4 bg-white">
      <div className="border-b-2 border-black mb-4 pb-2">
        <h2 className="text-xl font-bold tracking-tight">Messages</h2>
      </div>
      
      {loading ? (
        <div className="text-center py-8 text-black/70">
          Connexion à la base de données Mnesia...
        </div>
      ) : messages.length === 0 ? (
        <div className="text-center py-8 text-black/70">
          Aucun message disponible en Mnésie
        </div>
      ) : (
        messages.map((message) => (
          <Message key={message.id} message={message} />
        ))
      )}
    </div>
  );
};

export default MessageList;