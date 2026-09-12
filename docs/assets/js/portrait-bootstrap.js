/* Bootstrap resamples only highlights measured in the existing portrait. */
(() => {
  const reduced = matchMedia('(prefers-reduced-motion: reduce)');
  document.querySelectorAll('.portrait-plot[data-bootstrap]').forEach(figure => {
    const image = figure.querySelector('img');
    let ready = false, visible = false, frame = 0, last = 0;
    let phase = 0, previous = null, next = null, points = [], randomSeed = 571;
    const size = 700, duration = 2400;
    const random = () => { randomSeed = (1664525 * randomSeed + 1013904223) >>> 0; return (randomSeed + .5) / 4294967296; };
    const canvas = document.createElement('canvas');
    canvas.width = canvas.height = size;
    canvas.setAttribute('aria-hidden', 'true');
    canvas.className = 'portrait-bootstrap';
    const context = canvas.getContext('2d');

    function resample(reference = false) {
      const counts = new Uint16Array(points.length);
      if (reference) counts.fill(1);
      else for (let i = 0; i < points.length; i++) counts[Math.floor(random() * points.length)]++;
      const surface = document.createElement('canvas');
      surface.width = surface.height = size;
      const paint = surface.getContext('2d'), buckets = Array.from({length:32}, () => []);
      points.forEach((p, i) => { if (counts[i]) buckets[p.tone].push([p.x, p.y, Math.sqrt(counts[i]) * .675 * size / 485]); });
      buckets.forEach((bucket, i) => {
        paint.fillStyle = `rgba(242,244,241,${.04 + .96 * i / 31})`;
        paint.beginPath();
        bucket.forEach(([x,y,r]) => { paint.moveTo(x+r,y); paint.arc(x,y,r,0,Math.PI*2); });
        paint.fill();
      });
      return surface;
    }

    function draw(t) {
      // Weighted additive blend preserves tone between actual bootstrap samples.
      context.clearRect(0,0,size,size);
      context.globalCompositeOperation = 'source-over';
      context.globalAlpha = 1-t; context.drawImage(previous,0,0);
      context.globalCompositeOperation = 'lighter';
      context.globalAlpha = t; context.drawImage(next,0,0);
      context.globalAlpha = 1; context.globalCompositeOperation = 'source-over';
    }

    function tick(now) {
      if (!last) last = now;
      const delta = now-last;
      if (delta >= 50) {
        last=now; phase+=Math.min(delta,100);
        if (phase >= duration) {
          phase=0; previous=next; next=resample();
          figure.dataset.bootstrapCycle = String(Number(figure.dataset.bootstrapCycle || 0)+1);
        }
        const t=Math.min(1,Math.max(0,(phase-700)/1500));
        draw(t*t*(3-2*t));
      }
      frame=requestAnimationFrame(tick);
    }

    function sync() {
      cancelAnimationFrame(frame); last=0;
      const active = ready && !reduced.matches;
      figure.classList.toggle('bootstrap-ready',active);
      if(active && visible && !document.hidden) frame=requestAnimationFrame(tick);
    }

    function initialize() {
      if (ready || reduced.matches) { sync(); return; }
      try {
        const source=document.createElement('canvas');
        source.width=image.naturalWidth; source.height=image.naturalHeight;
        const read=source.getContext('2d',{willReadFrequently:true});
        read.drawImage(image,0,0);
        const {data}=read.getImageData(0,0,source.width,source.height);
        for(let y=0;y<source.height;y+=3) for(let x=0;x<source.width;x+=3) {
          let best=95,bx=0,by=0,sum=0,total=0;
          for(let dy=0;dy<3 && y+dy<source.height;dy++) for(let dx=0;dx<3 && x+dx<source.width;dx++) {
            const k=((y+dy)*source.width+x+dx)*4,r=data[k],g=data[k+1],b=data[k+2],v=(r+g+b)/3;
            sum+=v;total++;
            if(v>best && Math.max(r,g,b)-Math.min(r,g,b)<45) {best=v;bx=x+dx;by=y+dy;}
          }
          const tone=Math.max(0,Math.min(1,(sum/total-45)/175));
          if(best>95 && tone>.025) points.push({x:size*bx/source.width,y:size*by/source.height,tone:Math.min(31,Math.floor(Math.pow(tone,.85)*31))});
        }
        if(!points.length) return;
        previous=resample(true);next=resample();draw(0);
        figure.insertBefore(canvas,image.nextSibling);
        figure.dataset.pointCount=String(points.length);
        ready=true;sync();
      } catch(error) { console.warn('Portrait animation unavailable; keeping the original image.',error); }
    }

    new IntersectionObserver(entries=>{visible=entries[0].isIntersecting;sync();},{threshold:.15}).observe(figure);
    document.addEventListener('visibilitychange',sync);
    reduced.addEventListener('change',()=>{if(image.complete && image.naturalWidth) initialize(); else sync();});
    if(image.complete && image.naturalWidth) initialize(); else image.addEventListener('load',initialize,{once:true});
  });
})();
