import React from 'react';
import { Reply } from '../lib/mnesia';

interface ReplyListProps {
  replies: Reply[];
}

const ReplyList: React.FC<ReplyListProps> = ({ replies }) => {
  if (replies.length === 0) {
    return null;
  }

  return (
    <div className="mt-2 border-t border-gray-200 pt-2">
      <h4 className="text-sm mb-2 font-bold">Réponses:</h4>
      {replies.map((reply) => (
        <div key={reply.id} className="reply">
          <div className="flex justify-between items-center mb-1">
            <span className="font-bold">{reply.pseudo}</span>
            <span className="text-xs">ID: {reply.id}</span>
          </div>
          <p className="text-sm">{reply.text}</p>
        </div>
      ))}
    </div>
  );
};

export default ReplyList;