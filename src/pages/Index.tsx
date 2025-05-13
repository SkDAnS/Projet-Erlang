import React, { useEffect, useState } from 'react';
import MessageList from '@/components/MessageList';
import UserStatus from '@/components/UserStatus';
import logo from '../image/logo1.png';

const Index = () => {
  const [refreshTrigger, setRefreshTrigger] = useState(0);

 useEffect(() => {
  const interval = setInterval(() => {
    window.location.reload(); // Recharge complètement la page
  }, 30000); // Toutes les 5 secondes

  return () => clearInterval(interval);
}, []);


  return (
    <div className="min-h-screen" style={{ backgroundColor: '#386280' }}>
      <header className="p-4 shadow-md" style={{ backgroundColor: '#107BC7FF' }}>
        <div className="container mx-auto">
          <div className="flex flex-col md:flex-row justify-between items-start md:items-center gap-4">
            <div className="flex items-center gap-4">
              <img src={logo} alt="Logo" className="w-28 h-28 object-contain" />
              <span className="text-l font-serif text-black font-bold" style={{ fontFamily: "'Patrick Hand', sans-serif" }}>
              GHOSTINTHETHREAD
            </span>

            </div>
            <UserStatus />
          </div>
        </div>
      </header>

      <main className="container mx-auto py-6 px-4">
        <div className="max-w-3xl mx-auto">
          <MessageList key={refreshTrigger} />
        </div>
      </main>

      <footer className="mt-10 py-4 border-t" style={{ borderColor: '#395250', backgroundColor: '#107BC7FF'}}>
        <div className="container mx-auto text-center text-sm" style={{ color: '#000000FF'  }}>
          <p>Créé et fondé par l’équipe AMLRJB.</p>
        </div>
      </footer>
    </div>
  );
};

export default Index;
