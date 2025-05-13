import React from 'react';
import { Message as MessageType } from '../lib/mnesia';
import ReplyList from './ReplyList';

interface MessageProps {
  message: MessageType;
}

const Message: React.FC<MessageProps> = ({ message }) => {
  return (
    <div className="message-thread mb-6 p-4 border border-gray-300 rounded-md">
      <div className="message bg-white border shadow-sm rounded-md p-5">
        <div className="flex justify-between items-center mb-2">
          <span className="font-medium text-black">{message.pseudo}</span>
          <span className="text-xs text-gray-500">ID: {message.id}</span>
        </div>
        <p className="mb-4 text-gray-700 ml-4">{message.text}</p>  {/* Changer la couleur du texte du message en gris foncé */}
        
        <div className="flex items-center text-xs text-black mb-3">
          <span>❤️ {message.likes}</span>
        </div>
        
        <ReplyList replies={message.replies} />
      </div>
    </div>
  );
};

export default Message;
