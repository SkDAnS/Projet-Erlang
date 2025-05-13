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
    <div className="mt-2 border-t border-gray-300 pt-2">
      <h4 className="text-sm text-gray-700 mb-2">Réponses:</h4>
      {replies.map((reply) => (
        <div key={reply.id} className="reply ml-4 mb-2 p-2 bg-white rounded shadow-sm">
          <div className="flex justify-between items-center mb-1">
            <span className="font-medium text-black">{reply.pseudo}</span>
            <span className="text-xs text-gray-500">ID: {reply.id}</span>
          </div>
          {/* Texte du message décalé à gauche avec couleur gris foncé */}
          <p className="text-sm text-gray-700 ml-4">{reply.text}</p>
        </div>
      ))}
    </div>
  );
};

export default ReplyList;
