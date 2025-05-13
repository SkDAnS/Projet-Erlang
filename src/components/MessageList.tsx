import React, { useState, useEffect } from 'react';
import { Message as MessageType, mnesiaService } from '../lib/mnesia';
import Message from './Message';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { toast } from "sonner";

const MessageList: React.FC = () => {
  const [messages, setMessages] = useState<MessageType[]>([]);
  const [loading, setLoading] = useState<boolean>(true);

  useEffect(() => {
    // S'abonner aux mises à jour des messages
    const unsubscribe = mnesiaService.subscribeToMessages((newMessages) => {
      setMessages(newMessages);
      setLoading(false);
    });
    
    // Démarrer le polling pour les mises à jour
    mnesiaService.startPolling();
    
    // Afficher une notification de connexion
    toast.info("Connexion à la base de données Mnesia...");
    
    // Nettoyer l'abonnement et le polling lors du démontage du composant
    return () => {
      unsubscribe();
      mnesiaService.stopPolling();
    };
  }, []);

  return (
    <Card className="bg-blue-50 border-gray-300">
      <CardHeader>
        <CardTitle className="text-black">Messages Actuels</CardTitle>
      </CardHeader>
      <CardContent>
        {loading ? (
          <div className="text-center py-8 text-gray-500">
            Connexion à la base de données Mnesia...
          </div>
        ) : messages.length === 0 ? (
          <div className="text-center py-8 text-gray-500">
            Aucun message disponible dans Mnesia
          </div>
        ) : (
          messages.map((message) => (
            <Message key={message.id} message={message} />
          ))
        )}
      </CardContent>
    </Card>
  );
};

export default MessageList;
