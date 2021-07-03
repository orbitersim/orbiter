xof 0302txt 0064
template Header {
 <3D82AB43-62DA-11cf-AB39-0020AF71E433>
 WORD major;
 WORD minor;
 DWORD flags;
}

template Vector {
 <3D82AB5E-62DA-11cf-AB39-0020AF71E433>
 FLOAT x;
 FLOAT y;
 FLOAT z;
}

template Coords2d {
 <F6F23F44-7686-11cf-8F52-0040333594A3>
 FLOAT u;
 FLOAT v;
}

template Matrix4x4 {
 <F6F23F45-7686-11cf-8F52-0040333594A3>
 array FLOAT matrix[16];
}

template ColorRGBA {
 <35FF44E0-6C7C-11cf-8F52-0040333594A3>
 FLOAT red;
 FLOAT green;
 FLOAT blue;
 FLOAT alpha;
}

template ColorRGB {
 <D3E16E81-7835-11cf-8F52-0040333594A3>
 FLOAT red;
 FLOAT green;
 FLOAT blue;
}

template IndexedColor {
 <1630B820-7842-11cf-8F52-0040333594A3>
 DWORD index;
 ColorRGBA indexColor;
}

template Boolean {
 <4885AE61-78E8-11cf-8F52-0040333594A3>
 WORD truefalse;
}

template Boolean2d {
 <4885AE63-78E8-11cf-8F52-0040333594A3>
 Boolean u;
 Boolean v;
}

template MaterialWrap {
 <4885AE60-78E8-11cf-8F52-0040333594A3>
 Boolean u;
 Boolean v;
}

template TextureFilename {
 <A42790E1-7810-11cf-8F52-0040333594A3>
 STRING filename;
}

template Material {
 <3D82AB4D-62DA-11cf-AB39-0020AF71E433>
 ColorRGBA faceColor;
 FLOAT power;
 ColorRGB specularColor;
 ColorRGB emissiveColor;
 [...]
}

template MeshFace {
 <3D82AB5F-62DA-11cf-AB39-0020AF71E433>
 DWORD nFaceVertexIndices;
 array DWORD faceVertexIndices[nFaceVertexIndices];
}

template MeshFaceWraps {
 <4885AE62-78E8-11cf-8F52-0040333594A3>
 DWORD nFaceWrapValues;
 Boolean2d faceWrapValues;
}

template MeshTextureCoords {
 <F6F23F40-7686-11cf-8F52-0040333594A3>
 DWORD nTextureCoords;
 array Coords2d textureCoords[nTextureCoords];
}

template MeshMaterialList {
 <F6F23F42-7686-11cf-8F52-0040333594A3>
 DWORD nMaterials;
 DWORD nFaceIndexes;
 array DWORD faceIndexes[nFaceIndexes];
 [Material]
}

template MeshNormals {
 <F6F23F43-7686-11cf-8F52-0040333594A3>
 DWORD nNormals;
 array Vector normals[nNormals];
 DWORD nFaceNormals;
 array MeshFace faceNormals[nFaceNormals];
}

template MeshVertexColors {
 <1630B821-7842-11cf-8F52-0040333594A3>
 DWORD nVertexColors;
 array IndexedColor vertexColors[nVertexColors];
}

template Mesh {
 <3D82AB44-62DA-11cf-AB39-0020AF71E433>
 DWORD nVertices;
 array Vector vertices[nVertices];
 DWORD nFaces;
 array MeshFace faces[nFaces];
 [...]
}

template FrameTransformMatrix {
 <F6F23F41-7686-11cf-8F52-0040333594A3>
 Matrix4x4 frameMatrix;
}

template Frame {
 <3D82AB46-62DA-11cf-AB39-0020AF71E433>
 [...]
}

Header {
 1;
 0;
 1;
}

Frame CUBE_Root {
 FrameTransformMatrix {
  3.000000, 0.000000, 0.000000, 0.000000,
  0.000000, 3.000000, 0.000000, 0.000000,
  0.000000, 0.000000, 3.000000, 0.000000,
  0.000000, 0.000000, 0.000000, 1.000000;;
 }

Mesh Unnamed_0 {
 4;
 1.000000;1.000000;-1.000000;,    // 0
 -1.000000;1.000000;-1.000000;,   // 1
 -1.000000;1.000000;1.000000;,    // 2
 1.000000;1.000000;1.000000;;     // 3

 2;
 3;0,1,2;,
 3;0,2,3;;

  MeshMaterialList {
      1;
  2;
  0,
  0;
     Material {
     1.000000;1.000000;1.000000;1.000000;;
     0.000000;
     1.000000;1.000000;1.000000;;
       0.000000;0.000000;0.000000;;
          TextureFilename {
      "env0.bmp";
     }
     }
 }
  MeshNormals {
 4;
 0.000000;1.000000;0.000000;,
 0.000000;1.000000;0.000000;,
 0.000000;1.000000;0.000000;,
 0.000000;1.000000;0.000000;;

 2;
 3;0,1,2;,
 3;0,2,3;;
 }
  MeshTextureCoords {
    4;
    0.000000;0.000000;,
    0.000000;1.000000;,
    1.000000;1.000000;,
    1.000000;0.000000;;
 }
}

Mesh Unnamed_1 {
 4;
 1.000000;1.000000;-1.000000;,    // 0
 1.000000;1.000000;1.000000;,     // 3
 1.000000;-1.000000;1.000000;,    // 7
 1.000000;-1.000000;-1.000000;,   // 4

 2;
 3;0,1,2;,
 3;0,2,3;;

  MeshMaterialList {
      1;
  2;
  0,
  0;
     Material {
     1.000000;1.000000;1.000000;1.000000;;
     0.000000;
     1.000000;1.000000;1.000000;;
       0.000000;0.000000;0.000000;;
          TextureFilename {
      "env1.bmp";
     }
     }
 }
  MeshNormals {
 4;
 1.000000;0.000000;0.000000;,
 1.000000;0.000000;0.000000;,
 1.000000;0.000000;0.000000;,
 1.000000;0.000000;0.000000;,


 2;
 3;0,1,2;,
 3;0,2,3;;
 }
  MeshTextureCoords {
    4;
    0.000000;0.000000;,
    0.000000;1.000000;,
    1.000000;1.000000;,
    1.000000;0.000000;;
 }
}

Mesh Unnamed_2 {
 4;
 -1.000000;1.000000;-1.000000;,   // 1
 -1.000000;-1.000000;-1.000000;,  // 5
 -1.000000;-1.000000;1.000000;,   // 6
 -1.000000;1.000000;1.000000;,    // 2

 2;
 3;0,1,2;,
 3;0,2,3;;

  MeshMaterialList {
      1;
  2;
  0,
  0;
     Material {
     1.000000;1.000000;1.000000;1.000000;;
     0.000000;
     1.000000;1.000000;1.000000;;
       0.000000;0.000000;0.000000;;
          TextureFilename {
      "env2.bmp";
     }
     }
 }
  MeshNormals {
 4;
 -1.000000;0.000000;0.000000;,
 -1.000000;0.000000;0.000000;,
 -1.000000;0.000000;0.000000;,
 -1.000000;0.000000;0.000000;,


 2;
 3;0,1,2;,
 3;0,2,3;;
 }
  MeshTextureCoords {
    4;
    0.000000;0.000000;,
    0.000000;1.000000;,
    1.000000;1.000000;,
    1.000000;0.000000;;
 }
}

Mesh Unnamed_3 {
 4;
 1.000000;1.000000;1.000000;,     // 3
 -1.000000;1.000000;1.000000;,    // 2
 -1.000000;-1.000000;1.000000;,   // 6
 1.000000;-1.000000;1.000000;,    // 7

 2;
 3;0,1,2;,
 3;0,2,3;;

  MeshMaterialList {
      1;
  2;
  0,
  0;
     Material {
     1.000000;1.000000;1.000000;1.000000;;
     0.000000;
     1.000000;1.000000;1.000000;;
       0.000000;0.000000;0.000000;;
          TextureFilename {
      "env3.bmp";
     }
     }
 }
  MeshNormals {
 4;
 0.000000;0.000000;1.000000;,
 0.000000;0.000000;1.000000;,
 0.000000;0.000000;1.000000;,
 0.000000;0.000000;1.000000;,


 2;
 3;0,1,2;,
 3;0,2,3;;
 }
  MeshTextureCoords {
    4;
    0.000000;0.000000;,
    0.000000;1.000000;,
    1.000000;1.000000;,
    1.000000;0.000000;;
 }
}

Mesh Unnamed_4 {
 4;
 1.000000;1.000000;-1.000000;,    // 0
 1.000000;-1.000000;-1.000000;,   // 4
 -1.000000;-1.000000;-1.000000;,  // 5
 -1.000000;1.000000;-1.000000;,   // 1

 2;
 3;0,1,2;,
 3;0,2,3;;

  MeshMaterialList {
      1;
  2;
  0,
  0;
     Material {
     1.000000;1.000000;1.000000;1.000000;;
     0.000000;
     1.000000;1.000000;1.000000;;
       0.000000;0.000000;0.000000;;
          TextureFilename {
      "env4.bmp";
     }
     }
 }
  MeshNormals {
 4;
 0.000000;0.000000;-1.000000;,
 0.000000;0.000000;-1.000000;,
 0.000000;0.000000;-1.000000;,
 0.000000;0.000000;-1.000000;,


 2;
 3;0,1,2;,
 3;0,2,3;;
 }
  MeshTextureCoords {
    4;
    0.000000;0.000000;,
    0.000000;1.000000;,
    1.000000;1.000000;,
    1.000000;0.000000;;
 }
}

Mesh Unnamed_5 {
 4;
 1.000000;-1.000000;-1.000000;,   // 4
 1.000000;-1.000000;1.000000;,    // 7
 -1.000000;-1.000000;1.000000;,   // 6
 -1.000000;-1.000000;-1.000000;;  // 5

 2;
 3;0,1,2;,
 3;0,2,3;;

  MeshMaterialList {
      1;
  2;
  0,
  0;
     Material {
     1.000000;1.000000;1.000000;1.000000;;
     0.000000;
     1.000000;1.000000;1.000000;;
       0.000000;0.000000;0.000000;;
          TextureFilename {
      "env5.bmp";
     }
     }
 }
  MeshNormals {
 4;
 0.000000;-1.000000;0.000000;,
 0.000000;-1.000000;0.000000;,
 0.000000;-1.000000;0.000000;,
 0.000000;-1.000000;0.000000;;


 2;
 3;0,1,2;,
 3;0,2,3;;
 }
  MeshTextureCoords {
    4;
    0.000000;0.000000;,
    0.000000;1.000000;,
    1.000000;1.000000;,
    1.000000;0.000000;;
 }
}

}
