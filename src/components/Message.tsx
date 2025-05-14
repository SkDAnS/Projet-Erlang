import React from 'react';
import { Message as MessageType } from '../lib/mnesia';
import ReplyList from './ReplyList';

interface MessageProps {
  message: MessageType;
}

const Message: React.FC<MessageProps> = ({ message }) => {
  return (
    <div className="message-thread mb-6">
      <div className="message">
        <div className="message-header">
          <span className="font-bold">{message.pseudo}</span>
          <span className="text-xs">ID: {message.id}</span>
        </div>
        <p className="mb-4">{message.text}</p>
        
        <div className="flex items-center text-xs mb-3 border border-black inline-block px-2 py-1">
          <span>❤️ {message.likes}</span>
        </div>
        
        <ReplyList replies={message.replies} />
      </div>
    </div>
  );
};

export default Message;