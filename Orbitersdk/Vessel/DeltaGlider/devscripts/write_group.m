% Write a mesh group to a stream

function write_group (fid, vtx, idx, matidx, texidx, label, flag)
    global g;
    nvtx = size(vtx,1);
    nn   = size(vtx,2);
    nidx = size(idx,1);
    fprintf(fid, 'MATERIAL %d\n', matidx);
    fprintf(fid, 'TEXTURE %d\n', texidx);
    if nargin > 6
        fprintf(fid, 'FLAG %d\n', flag);
    end
    if nargin > 5
        fprintf(fid, 'LABEL %s\n', label);
    end
    fprintf(fid, 'GEOM %d %d\n', nvtx, nidx);
    for i=1:nvtx
        for j=1:nn
            fprintf(fid, '%0.5f', vtx(i,j));
            if j==nn
                fprintf(fid, '\n');
            else
                fprintf(fid, ' ');
            end
        end
    end
    for i=1:nidx
        fprintf(fid, '%d %d %d\n', idx(i,1), idx(i,2), idx(i,3));
    end

    if isstruct(g)
        if isfield(g, 'ngroup')
            g.ngroup = g.ngroup+1;
        else
            g.ngroup = 1;
        end
        if isfield(g, 'nvtx')
            g.nvtx = g.nvtx + nvtx;
        else
            g.nvtx = nvtx;
        end
        if matidx > 0
            g.mat_used(matidx) = 1;
        end
        if texidx > 0
            g.tex_used(texidx) = 1;
        end
    end
end