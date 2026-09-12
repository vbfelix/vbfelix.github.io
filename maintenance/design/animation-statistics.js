// Every mark starts at a measured highlight in the actual portrait.
const $=id=>document.getElementById(id),c=$('photo-points'),ctx=c.getContext('2d');
const variants={
 converge:{heading:'Amostragem que revela seu rosto.',notes:'Uma amostra aleatória sem reposição cresce de 2% até 100% dos pontos extraídos da foto. O rosto aparece porque os próprios pontos do retrato entram na amostra.',usage:'Sugestão para a home: uma entrada de 5 segundos, encerrando com o retrato completo. Coordenadas e intensidades vêm da imagem.',duration:5000},
 scan:{heading:'Bootstrap do próprio retrato.',notes:'Os pontos da foto são sorteados com reposição. Alguns ficam de fora; outros aparecem repetidas vezes e ganham peso visual. Cada reamostragem tem o mesmo tamanho do conjunto original.',usage:'São 16 reamostragens. O tamanho de cada ponto acompanha a raiz de sua multiplicidade. Ao terminar, a referência completa retorna.',duration:8000},
 breathe:{heading:'Incerteza nas coordenadas.',notes:'Os próprios pontos do retrato recebem erros gaussianos nas posições x e y. A imagem perde definição quando o desvio-padrão cresce; cada ponto retorna à sua posição quando ele volta a zero.',usage:'Posição observada = posição original + ε, com ε ~ N(0, σ²) independente por eixo. σ varia de 0 a 12 unidades do gráfico e retorna a zero.',duration:6500}
};
let seed=571;const rand=()=>{seed=(1664525*seed+1013904223)>>>0;return(seed+.5)/4294967296;};
const normal=()=>Math.sqrt(-2*Math.log(rand()))*Math.cos(2*Math.PI*rand());
let points=[],order=[],samples=[],mode='scan',frame=0,start=0,elapsed=0,playing=false,lastDraw=0;
const reduced=matchMedia('(prefers-reduced-motion: reduce)'),fmt=n=>n.toLocaleString('pt-BR');
function paint(t=1,reference=false){
 const N=points.length;if(!N)return;ctx.setTransform(1.5,0,0,1.5,0,0);ctx.clearRect(0,0,600,600);
 ctx.strokeStyle='#648477';ctx.globalAlpha=.25;ctx.lineWidth=.6;ctx.beginPath();
 for(let a=140;a<535;a+=100){ctx.moveTo(65,a);ctx.lineTo(555,a);}for(let a=165;a<535;a+=100){ctx.moveTo(a,50);ctx.lineTo(a,535);}ctx.stroke();ctx.globalAlpha=1;
 const count=reference?N:mode==='converge'?Math.min(N,Math.max(1,Math.round(N*(.02+.98*t)))):N;
 const b=Math.min(15,Math.floor(t*16)),bootstrap=mode==='scan'&&!reference&&t<1,weights=bootstrap?samples[b]:null;
 const sigma=mode==='breathe'&&!reference?12*Math.sin(Math.PI*t)**2:0;
 const buckets=Array.from({length:32},()=>[]);let unique=0;
 for(let i=0;i<count;i++){const k=mode==='converge'&&!reference?order[i]:i,p=points[k],w=weights?weights[k]:1;if(!w)continue;unique++;const size=p.size*(bootstrap?Math.sqrt(w):1);buckets[Math.min(31,Math.floor(p.intensity*31))].push([p.x+sigma*p.zx,p.y+sigma*p.zy,size]);}
 for(let j=0;j<32;j++){ctx.fillStyle=`rgba(242,244,241,${.04+.96*j/31})`;ctx.beginPath();for(const[x,y,d]of buckets[j]){ctx.moveTo(x+d/2,y);ctx.arc(x,y,d/2,0,Math.PI*2);}ctx.fill();}
 ctx.strokeStyle='#bdcdbf';ctx.lineWidth=1;ctx.beginPath();ctx.moveTo(65,42);ctx.lineTo(65,535);ctx.lineTo(565,535);ctx.stroke();ctx.fillStyle='#bdcdbf';ctx.font='italic 18px Georgia';ctx.fillText('x',573,542);ctx.fillText('y',57,28);ctx.fillText('0',45,557);ctx.fillText('(xᵢ, yᵢ)',444,33);
 const caption=reference?'Referência: todos os pontos da foto':mode==='converge'?`${Math.round(count/N*100)}% · ${fmt(count)} de ${fmt(N)} pontos`:mode==='scan'?(bootstrap?`Bootstrap ${b+1}/16 · ${Math.round(unique/N*100)}% distintos`:'Referência completa restaurada'):`σ = ${sigma.toFixed(1)} · mesmos ${fmt(N)} pontos`;
 ctx.fillStyle='#efd58e';ctx.font='15px monospace';ctx.fillText(caption,68,580);
 c.dataset.pointCount=N;c.dataset.visibleCount=unique;c.dataset.mode=mode;c.dataset.progress=t.toFixed(3);
}
function stop(){cancelAnimationFrame(frame);playing=false;$('pause').disabled=true;$('pause').textContent='Pausar';}
function tick(now){elapsed=now-start;const t=Math.min(1,elapsed/variants[mode].duration);if(now-lastDraw>32||t===1){paint(t);lastDraw=now;}if(t<1)frame=requestAnimationFrame(tick);else{stop();$('status').textContent='Concluído. O retrato completo permanece visível.';}}
function play(){if(!points.length)return;stop();if(reduced.matches){paint(1,true);$('status').textContent='Movimento reduzido: retrato completo.';return;}playing=true;elapsed=0;start=performance.now();lastDraw=0;$('pause').disabled=false;$('status').textContent='Animação dos pontos da foto em andamento.';frame=requestAnimationFrame(tick);}
function select(button){stop();mode=button.dataset.mode;document.querySelectorAll('[data-mode]').forEach(b=>b.setAttribute('aria-pressed',b===button));for(const key of['heading','notes','usage'])$(key).textContent=variants[mode][key];paint(1,true);$('status').textContent=points.length?`${fmt(points.length)} pontos extraídos do retrato. Clique em Reproduzir.`:'Extraindo os pontos da foto…';}
document.querySelectorAll('[data-mode]').forEach(b=>b.onclick=()=>select(b));$('play').disabled=true;$('play').onclick=play;
$('pause').onclick=()=>{if(playing){cancelAnimationFrame(frame);playing=false;$('pause').textContent='Continuar';$('status').textContent='Pausado.';}else{playing=true;start=performance.now()-elapsed;$('pause').textContent='Pausar';frame=requestAnimationFrame(tick);}};
reduced.addEventListener('change',()=>{stop();paint(1,true);});document.addEventListener('visibilitychange',()=>{if(document.hidden){stop();paint(1,true);}});
const image=new Image();image.onload=()=>{
 const source=document.createElement('canvas');source.width=image.naturalWidth;source.height=image.naturalHeight;const sc=source.getContext('2d',{willReadFrequently:true});sc.drawImage(image,0,0);const{data}=sc.getImageData(0,0,source.width,source.height);
 // Preserve the brightest point's position, but measure local mean tone to
 // avoid flattening facial shadows. Smaller cells retain glasses and eyes.
 const cell=3;
 for(let y=0;y<source.height;y+=cell)for(let x=0;x<source.width;x+=cell){
   let best=95,bx=0,by=0,sum=0,total=0;
   for(let dy=0;dy<cell&&y+dy<source.height;dy++)for(let dx=0;dx<cell&&x+dx<source.width;dx++){
     const k=((y+dy)*source.width+x+dx)*4,r=data[k],g=data[k+1],b=data[k+2],v=(r+g+b)/3;
     sum+=v;total++;
     if(v>best&&Math.max(r,g,b)-Math.min(r,g,b)<45){best=v;bx=x+dx;by=y+dy;}
   }
   const tone=Math.max(0,Math.min(1,(sum/total-45)/175));
   if(best>95&&tone>.025)points.push({x:70+485*bx/source.width,y:50+485*by/source.height,intensity:Math.pow(tone,.85),size:1.35,zx:normal(),zy:normal()});
 }
 order=points.map((_,i)=>i);for(let i=order.length-1;i>0;i--){const j=Math.floor(rand()*(i+1));[order[i],order[j]]=[order[j],order[i]];}
 samples=Array.from({length:16},()=>{const counts=new Uint16Array(points.length);for(let i=0;i<points.length;i++)counts[Math.floor(rand()*points.length)]++;return counts;});
 c.dataset.bootstrapValid=samples.every(s=>s.reduce((sum,n)=>sum+n,0)===points.length);$('play').disabled=false;select(document.querySelector(`[data-mode="${mode}"]`));
};
image.onerror=()=>{$('status').textContent='Não foi possível carregar o retrato. Reabra a prévia pelo servidor local.';};
select(document.querySelector('[data-mode="scan"]'));image.src='../../images/portrait-scatter-gray.png';
