% =========================================================================
% Add vertices mvtx and faces midx to mesh group vtx,idx
function [vtx,idx] = add_to_group(vtx,idx,mvtx,midx)
    idx = [idx;midx + size(vtx,1)];
    vtx = [vtx;mvtx];
end