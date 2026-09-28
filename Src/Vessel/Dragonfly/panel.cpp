// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#ifndef __linux__
#include "panel.h"
#include <windows.h>
#include < GL\gl.h >
#else // __linux__
#include "Panel.h"
// windows.h left out: GDI drawing is QPainter, the pens, brushes, fonts and bitmaps are Qt objects
#include <QPainter>
#include <GL/gl.h>
#endif // __linux__
#include <math.h>
#include <stdio.h>
#ifndef __linux__
#include "orbitersdk.h"
#else // __linux__
#include <stdlib.h>
#include <string.h>
#include "Orbitersdk.h"
#include "OrbiterResource.h"
#endif // __linux__
#include "resource.h"

#ifndef __linux__
HFONT hFNT_Panel;
HBRUSH hBRUSH_Black,hBRUSH_Yellow,hBRUSH_BYellow,hBRUSH_Red,hBRUSH_Green,hBRUSH_Background,hBRUSH_LBkg;
HBRUSH hBRUSH_TotalBlack;
HBRUSH hBRUSH_FYellow,hBRUSH_Gray;
HBRUSH hBRUSH_White,hBRUSH_StrpWht;
HBRUSH hBRUSH_Brown,hBRUSH_Sky;
HPEN hPEN_White,hPEN_Gray,hPEN_Black,hPEN_NULL,hPEN_Cyan,hPEN_BYellow;
HPEN hPEN_LGray;
HBITMAP hBITMAP_ADI;
#else // __linux__
QFont *hFNT_Panel;
QBrush *hBRUSH_Black,*hBRUSH_Yellow,*hBRUSH_BYellow,*hBRUSH_Red,*hBRUSH_Green,*hBRUSH_Background,*hBRUSH_LBkg;
QBrush *hBRUSH_TotalBlack;
QBrush *hBRUSH_FYellow,*hBRUSH_Gray;
QBrush *hBRUSH_White,*hBRUSH_StrpWht;
QBrush *hBRUSH_Brown,*hBRUSH_Sky;
QPen *hPEN_White,*hPEN_Gray,*hPEN_Black,*hPEN_NULL,*hPEN_Cyan,*hPEN_BYellow;
QPen *hPEN_LGray;
QImage *hBITMAP_ADI;
#endif // __linux__
SURFHANDLE hClockSRF;
SURFHANDLE hSwitchSRF;
SURFHANDLE hHgaugeSRF;
SURFHANDLE hEgaugeSRF;
SURFHANDLE hRotarySRF;
SURFHANDLE hTbSRF;
SURFHANDLE hCbSRF;
SURFHANDLE hSliderSRF;
SURFHANDLE hCwSRF;
SURFHANDLE hMFDSRF;
SURFHANDLE hDockBSRF;
SURFHANDLE hDockSW1SRF;
SURFHANDLE hDockDlSRF;
SURFHANDLE hDockSW2SRF;
SURFHANDLE hNavSRF;
SURFHANDLE hRadarSRF;
SURFHANDLE hRadBkSRF;
SURFHANDLE hRadSrfSRF;
SURFHANDLE hFuelSRF;
SURFHANDLE hVrotSRF;
SURFHANDLE hVrotBkSRF;
SURFHANDLE hADIBorder;
SURFHANDLE hFront_Panel_SRF[7];
bool Panel_Resources_Loaded;

#ifdef __linux__
// not upstream: the wingdi.h BMP file structures (file layout; the file header is 2-byte packed as in wingdi.h)
#pragma pack(push, 2)
struct BITMAPFILEHEADER { WORD bfType; DWORD bfSize; WORD bfReserved1; WORD bfReserved2; DWORD bfOffBits; };
#pragma pack(pop)
struct BITMAPINFOHEADER { DWORD biSize; LONG biWidth; LONG biHeight; WORD biPlanes; WORD biBitCount; DWORD biCompression; DWORD biSizeImage; LONG biXPelsPerMeter; LONG biYPelsPerMeter; DWORD biClrUsed; DWORD biClrImportant; };
struct RGBTRIPLE { BYTE rgbtBlue; BYTE rgbtGreen; BYTE rgbtRed; };

#endif // __linux__
int LoadOGLBitmap(const char *filename)
{
   unsigned char *l_texture;
   int l_index, l_index2=0;
   FILE *file;
   BITMAPFILEHEADER fileheader; 
   BITMAPINFOHEADER infoheader;
   RGBTRIPLE rgb; 
   int num_texture=1; //we only use one OGL texture ,so...
   

   if( (file = fopen(filename, "rb"))==NULL) return (-1); 
   fread(&fileheader, sizeof(fileheader), 1, file); 
   fseek(file, sizeof(fileheader), SEEK_SET);
   fread(&infoheader, sizeof(infoheader), 1, file);

#ifndef __linux__
   l_texture = (byte *) malloc(infoheader.biWidth * infoheader.biHeight * 4);
#else // __linux__
   l_texture = (unsigned char *) malloc(infoheader.biWidth * infoheader.biHeight * 4);
#endif // __linux__
   memset(l_texture, 0, infoheader.biWidth * infoheader.biHeight * 4);

   for (l_index=0; l_index < infoheader.biWidth*infoheader.biHeight; l_index++)
   { 
      fread(&rgb, sizeof(rgb), 1, file); 

      l_texture[l_index2+0] = rgb.rgbtRed; // Red component
      l_texture[l_index2+1] = rgb.rgbtGreen; // Green component
      l_texture[l_index2+2] = rgb.rgbtBlue; // Blue component
      l_texture[l_index2+3] = 255; // Alpha value
      l_index2 += 4; // Go to the next position
   }

   fclose(file); // Closes the file stream

   glBindTexture(GL_TEXTURE_2D, num_texture);
   
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
   //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); 
   glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
   glTexImage2D(GL_TEXTURE_2D, 0, 4, infoheader.biWidth, infoheader.biHeight, 
	                               0, GL_RGBA, GL_UNSIGNED_BYTE, l_texture);
   


   free(l_texture); 

return (num_texture);
};


#ifndef __linux__
void MoveTo(HDC PANEL_hdc, int x, int y)
{ HPEN oldp;
  oldp=static_cast <HPEN> (SelectObject(PANEL_hdc,hPEN_NULL));
  LineTo(PANEL_hdc,x,y);
  SelectObject(PANEL_hdc,oldp);
#else // __linux__
QPoint PANEL_cp; // not upstream: the current position GDI keeps in the DC (QPainter has none)

void MoveTo(QPainter *PANEL_hdc, int x, int y)
{ PANEL_cp=QPoint(x,y); // LineTo with the NULL pen: only the current position moves
};

// not upstream: LineTo counterpart, a line from the current position that becomes the new current position
void DrawLineTo(QPainter *PANEL_hdc, int x, int y)
{ PANEL_hdc->drawLine(PANEL_cp,QPoint(x,y));
  PANEL_cp=QPoint(x,y);
};

// not upstream: TextOut counterpart; GDI draws text in the text colour (not the pen), x aligned by SetTextAlign, y the top edge
void DrawTextOut(QPainter *PANEL_hdc, int x, int y, const char *str, int len, const QColor &textcol, int talign)
{ QPen pen=PANEL_hdc->pen();
  PANEL_hdc->setPen(textcol);
  PANEL_hdc->drawText(QRect(x,y,0,0),talign|Qt::TextDontClip,QString::fromLatin1(str,len));
  PANEL_hdc->setPen(pen);
#endif // __linux__
};
Panel::Panel()
{
	instruments=NULL;
	screw_num = 0;
#ifdef __linux__
	hDC3=NULL; // not upstream: DeleteDC ignored a stale handle of a panel never shown, delete does not
#endif // __linux__
};

Panel::~Panel()
#ifndef __linux__
{ DeleteDC(hDC3);
#else // __linux__
{ delete hDC3;
#endif // __linux__
  instrument_list *runner=instruments;
  instrument_list *gone;
  while (runner){ gone=runner;
				  runner=runner->next;
				  delete gone->instance;
				  delete gone;
  };
};
void Panel::MakeYourBackground()
{ 
	surf=oapiCreateSurface(Wdth,Hght);
	hDC=oapiGetDC(surf);
#ifndef __linux__
	hDC2=CreateCompatibleDC(hDC);
	hDC3=CreateCompatibleDC(hDC);
	hBitmap=CreateCompatibleBitmap(hDC,Wdth,Hght);
	HBITMAP hBitmapOld=(HBITMAP)SelectObject(hDC2,hBitmap);
	DeleteObject(hBitmapOld);
	SelectObject(hDC2,hBRUSH_Background);
	Rectangle(hDC2,0,0,Wdth,Hght);
#else // __linux__
	hDC2=new QPainter; // CreateCompatibleDC: a memory DC draws once a bitmap is selected
	hDC3=new QPainter;
	hBitmap=new QImage(Wdth,Hght,QImage::Format_RGB32); // CreateCompatibleBitmap; oapiRegisterPanelBackground deletes it
	hDC2->begin(hBitmap); // SelectObject (hDC2, hBitmap)
	// DeleteObject of the replaced stock bitmap left out: QPainter has none
	hDC2->setBrush(*hBRUSH_Background);
	hDC2->drawRect(0,0,Wdth-1,Hght-1); // Rectangle: right and bottom edges are exclusive
#endif // __linux__
	Panel::NowPutScrews();
	Panel::NowPutTextOnBackground();
	Panel::NowPutCText();
	Panel::NowPutBorders();
#ifndef __linux__
	DeleteDC(hDC2);
#else // __linux__
	delete hDC2; // DeleteDC (ends the painting)
#endif // __linux__
	oapiReleaseDC(surf,hDC);
	oapiDestroySurface(surf);

	
}
void Panel::NowPutScrews()
{int di=5*sqrt(2.0)/2;
#ifndef __linux__
 SelectObject(hDC2,hPEN_White);
 SelectObject(hDC2,hBRUSH_Background);
#else // __linux__
 hDC2->setPen(*hPEN_White);
 hDC2->setBrush(*hBRUSH_Background);
#endif // __linux__
 int x,y;
	for (int i=0;i<screw_num;i++)
	{  x=Screw_list[i].x;
	   y=Screw_list[i].y;
#ifndef __linux__
	   Ellipse(hDC2,x-5,y-5,x+5,y+5);
	   MoveTo(hDC2,x-di,y-di);LineTo(hDC2,x+di,y+di);
#else // __linux__
	   hDC2->drawEllipse(x-5,y-5,9,9); // Ellipse (x-5,y-5,x+5,y+5): right and bottom edges are exclusive
	   MoveTo(hDC2,x-di,y-di);DrawLineTo(hDC2,x+di,y+di);
#endif // __linux__
	}
};
void Panel::NowPutTextOnBackground()
#ifndef __linux__
{  SetBkMode(hDC2,TRANSPARENT); 
#else // __linux__
{  hDC2->setBackgroundMode(Qt::TransparentMode); 
#endif // __linux__
 //  SetTextColor(hDC2,RGB(0,147,221));
#ifndef __linux__
SetTextColor(hDC2,RGB(255,100,100));
   SetTextAlign(hDC2,TA_CENTER);
   SelectObject(hDC2,hFNT_Panel);
#else // __linux__
hDC2->setPen(QColor(255,100,100)); // SetTextColor: QPainter draws text with the pen
   // SetTextAlign (TA_CENTER): Qt::AlignHCenter at the drawText below
   hDC2->setFont(*hFNT_Panel);
#endif // __linux__
   for (int i=0;i<text_num;i++)
#ifndef __linux__
	TextOut(hDC2,Text_list[i].x,Text_list[i].y,Text_list[i].text, sizeof(char)*strlen(Text_list[i].text));
#else // __linux__
	hDC2->drawText(QRect(Text_list[i].x,Text_list[i].y,0,0),Qt::AlignHCenter|Qt::TextDontClip,QString::fromLatin1(Text_list[i].text, sizeof(char)*strlen(Text_list[i].text)));
#endif // __linux__
  
}
void Panel::NowPutCText()
{ 
#ifndef __linux__
  SetBkMode(hDC2,TRANSPARENT); 
#else // __linux__
  hDC2->setBackgroundMode(Qt::TransparentMode); 
#endif // __linux__
  //SetTextColor(hDC2,RGB(0,147,221));
#ifndef __linux__
  SetTextColor(hDC2,RGB(255,100,100));
  SelectObject(hDC2,hPEN_Cyan);
  SetTextAlign(hDC2,TA_CENTER);SetBkColor(hDC2,RGB(145,48,48));
  SetBkMode(hDC2,OPAQUE);
  SelectObject(hDC2,hFNT_Panel);
#else // __linux__
  // SetTextColor (RGB(255,100,100)): the colour of hPEN_Cyan, the pen QPainter draws the text with
  hDC2->setPen(*hPEN_Cyan);
  // SetTextAlign (TA_CENTER): Qt::AlignHCenter at the drawText below
  hDC2->setBackground(QColor(145,48,48));
  hDC2->setBackgroundMode(Qt::OpaqueMode);
  hDC2->setFont(*hFNT_Panel);
#endif // __linux__
  for (int i=0;i<ctext_num;i++) {  
#ifndef __linux__
  MoveTo (hDC2,CText_list[i].x+CText_list[i].lngth/2, CText_list[i].y+CText_list[i].dir*7+5);LineTo(hDC2,CText_list[i].x+CText_list[i].lngth/2,CText_list[i].y+5);
  LineTo(hDC2,CText_list[i].x-CText_list[i].lngth/2,CText_list[i].y+5);LineTo(hDC2,CText_list[i].x-CText_list[i].lngth/2,CText_list[i].y+CText_list[i].dir*7+5);
#else // __linux__
  MoveTo (hDC2,CText_list[i].x+CText_list[i].lngth/2, CText_list[i].y+CText_list[i].dir*7+5);DrawLineTo(hDC2,CText_list[i].x+CText_list[i].lngth/2,CText_list[i].y+5);
  DrawLineTo(hDC2,CText_list[i].x-CText_list[i].lngth/2,CText_list[i].y+5);DrawLineTo(hDC2,CText_list[i].x-CText_list[i].lngth/2,CText_list[i].y+CText_list[i].dir*7+5);
#endif // __linux__
 
#ifndef __linux__
  TextOut(hDC2,CText_list[i].x,CText_list[i].y,CText_list[i].text, sizeof(char)*strlen(CText_list[i].text));
#else // __linux__
  hDC2->drawText(QRect(CText_list[i].x,CText_list[i].y,0,0),Qt::AlignHCenter|Qt::TextDontClip,QString::fromLatin1(CText_list[i].text, sizeof(char)*strlen(CText_list[i].text)));
#endif // __linux__
  }
};
  
void Panel::NowPutBorders()
{ int px,py,nr;
#ifndef __linux__
 SetBkMode(hDC2,TRANSPARENT); 
 SelectObject(hDC2,hPEN_Black);
#else // __linux__
 hDC2->setBackgroundMode(Qt::TransparentMode); 
 hDC2->setPen(*hPEN_Black);
#endif // __linux__
 for (int i=0;i<border_num;i++){
 px=Border_list[i].x;py=Border_list[i].y;
 nr=Border_list[i].num;

#ifndef __linux__
 Arc(hDC2,px-5,py-5,px+6,py+6,px,py-5,px-5,py);
 MoveTo(hDC2,px-5,py);LineTo(hDC2,px-5,py+40);py+=40;
 Arc(hDC2,px-5,py-5,px+6,py+6,px-5,py,px,py+5);
 MoveTo(hDC2,px,py+5);LineTo(hDC2,px+45*nr+5,py+5);px+=45*nr+5;
 Arc(hDC2,px-5,py-5,px+6,py+6,px,py+5,px+5,py);
 MoveTo(hDC2,px+5,py);LineTo(hDC2,px+5,py-40);py-=40;
 Arc(hDC2,px-5,py-5,px+6,py+6,px+5,py,px,py-5);
 MoveTo(hDC2,px,py-5);LineTo(hDC2,px-45*nr-5,py-5);
#else // __linux__
 // Arc (px-5,py-5,px+6,py+6, start, end): counterclockwise quarter circles of radius 5 around (px,py), angles in 1/16 degree
 hDC2->drawArc(px-5,py-5,10,10,90*16,90*16);
 MoveTo(hDC2,px-5,py);DrawLineTo(hDC2,px-5,py+40);py+=40;
 hDC2->drawArc(px-5,py-5,10,10,180*16,90*16);
 MoveTo(hDC2,px,py+5);DrawLineTo(hDC2,px+45*nr+5,py+5);px+=45*nr+5;
 hDC2->drawArc(px-5,py-5,10,10,270*16,90*16);
 MoveTo(hDC2,px+5,py);DrawLineTo(hDC2,px+5,py-40);py-=40;
 hDC2->drawArc(px-5,py-5,10,10,0,90*16);
 MoveTo(hDC2,px,py-5);DrawLineTo(hDC2,px-45*nr-5,py-5);
#endif // __linux__
 }
}
void Panel::AddInstrument(instrument *new_inst)
{ instrument_list *new_item=new instrument_list;
  new_item->instance=new_inst;
  new_item->next=instruments;
  instruments=new_item;
};
void Panel::RegisterYourInstruments()
{  instrument_list *runner=instruments;
  int index=0;
  while (runner)
  { if (runner->instance)
			runner->instance->RegisterMe(index++); //now register all the inst. in the list
    runner=runner->next;
  }
};
void Panel::Paint(int index)
{ instrument_list *runner=instruments;
   for (int i=0;i<index;i++) runner=runner->next;
   runner->instance->PaintMe();

}
void Panel::Refresh(int index)
{ instrument_list *runner=instruments;
   for (int i=0;i<index;i++) runner=runner->next;
   runner->instance->RefreshMe();

}
void Panel::LBD(int index,int x,int y)
{ instrument_list *runner=instruments;
   for (int i=0;i<index;i++) runner=runner->next;
   runner->instance->LBD( x, y);

}
void Panel::RBD(int index,int x,int y)
{ instrument_list *runner=instruments;
   for (int i=0;i<index;i++) runner=runner->next;
   runner->instance->RBD( x, y);

}
void Panel::BU(int index)
{ instrument_list *runner=instruments;
   for (int i=0;i<index;i++) runner=runner->next;
   runner->instance->BU();

}

void Panel::Save(FILEHANDLE scn)
{ instrument_list *runner=instruments;
  char cbuf[80];
  int int_sv[10];
  int index=0;
 //save the switches 
  while (runner)
  { if ((runner->instance)&&(runner->instance->type==33))
		    int_sv[index++]=((Switch*)runner->instance)->pos;
 		    runner=runner->next;
			//every 5 switches save
			if (index==5) {sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	                       oapiWriteScenario_string (scn, (char*)"    SW ", cbuf);
						   index=0;
						}
	};
  //end of inst list. dump the rest
  if (index>0)
		{
		int_sv[index]=99;
		sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	    oapiWriteScenario_string (scn, (char*)"    SW ", cbuf);
		index=0;
		};
  cbuf[0]=0;
  oapiWriteScenario_string (scn, (char*)"  SW END ", cbuf);
  //save the rotary
  runner=instruments;
  index=0;
  while (runner)
  { if ((runner->instance)&&(runner->instance->type==34))
		    int_sv[index++]=((Rotary*)runner->instance)->set;
 		    runner=runner->next;
			//every 5 switches save
			if (index==5) {sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	                       oapiWriteScenario_string (scn, (char*)"    ROT ", cbuf);
						   index=0;
						}
	};
  //end of inst list. dump the rest
  if (index>0)
		{
		int_sv[index]=99;
		sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	    oapiWriteScenario_string (scn, (char*)"    ROT ", cbuf);
		index=0;
		};
  cbuf[0]=0;
  oapiWriteScenario_string (scn, (char*)"  ROT END ", cbuf);
  runner=instruments;
  //index=0;
  while (runner)
  { if ((runner->instance)&&(runner->instance->type==44))//ADI ball saving
			{
			ADI* adi_p=(ADI*)runner->instance;
			sprintf(cbuf,"%0.4f %0.4f %0.4f %i %i %0.4f %0.4f %0.4f",adi_p->now.x,adi_p->now.y,adi_p->now.z,
															    adi_p->orbital_ecliptic,adi_p->function_mode,
																adi_p->reference.x,adi_p->reference.y,adi_p->reference.z);
			oapiWriteScenario_string (scn, (char*)"    ADI ", cbuf);
			};//end of if
    runner=runner->next;
  };//end of while
  cbuf[0]=0;
  oapiWriteScenario_string (scn, (char*)"  PAN END ", cbuf);
}
void Panel::Load(FILEHANDLE scn)
{ instrument_list *runner=instruments;
 char *line;
 int int_sv[10];
 //int index=0;
 //read the switches	
 oapiReadScenario_nextline (scn, line);
 while (strncmp(line,"SW END",6))
		{ sscanf (line,"    SW %i %i %i %i %i", &int_sv[0],&int_sv[1],&int_sv[2],&int_sv[3],&int_sv[4]);
	      for (int i=0;i<5;i++)
		  {   while ((runner) && (runner->instance->type!=33)) runner=runner->next;
			   ((Switch*)runner->instance)->pos=int_sv[i];
			   if (int_sv[i+1]==99) break;
			   runner=runner->next;
		  }
 oapiReadScenario_nextline (scn, line);
 }
//read the rotaries
 runner=instruments;
  oapiReadScenario_nextline (scn, line);
 while (strncmp(line,"ROT END",6))
		{ sscanf (line,"    ROT %i %i %i %i %i", &int_sv[0],&int_sv[1],&int_sv[2],&int_sv[3],&int_sv[4]);
	      for (int i=0;i<5;i++)
		  {   while ((runner) && (runner->instance->type!=34)) runner=runner->next;
			   ((Rotary*)runner->instance)->set=int_sv[i];
			   if (int_sv[i+1]==99) break;
			   runner=runner->next;
		  }
 oapiReadScenario_nextline (scn, line);
 }
//read the extra instruments info
runner=instruments;
  oapiReadScenario_nextline (scn, line);
 while (strncmp(line,"PAN END",6))	//until we clear this panel
	{ if (!strncmp(line,"ADI",3)) //we have an adi to load
		{ while ((runner) && (runner->instance->type!=44)) runner=runner->next;//go to ADI 
			ADI*  adi_p=(ADI*)runner->instance;
         sscanf(line,"    ADI %lf %lf %lf %i %i %lf %lf %lf",&(adi_p->now.x),&(adi_p->now.y),&(adi_p->now.z),
													   &(adi_p->orbital_ecliptic),&(adi_p->function_mode),
														&(adi_p->reference.x),&(adi_p->reference.y),&(adi_p->reference.z));
		};//end of adi found !
	oapiReadScenario_nextline (scn, line);	
	};//end of while
};


#ifndef __linux__
LRESULT WINAPI MsgProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
    switch( msg )
    {
        case WM_DESTROY:
            PostQuitMessage( 0 );
            return 0;

        case WM_PAINT:
            return 0;
    }

    return DefWindowProc( hWnd, msg, wParam, lParam );
}
#else // __linux__
// MsgProc left out: window procedure of an OpenGL window that is never created (CreateGLWindow is commented out)
#endif // __linux__

void PANEL_DLLAtach()
{Panel_Resources_Loaded=0;}


#ifndef __linux__
void PANEL_InitGDIResources(HINSTANCE hModule)
#else // __linux__
void PANEL_InitGDIResources(void *hModule)
#endif // __linux__
{ if (Panel_Resources_Loaded) return;
#ifndef __linux__
  hPEN_White=CreatePen(PS_SOLID,1,RGB(250,250,250));
  hPEN_Gray=CreatePen(PS_SOLID,1,RGB(100,100,100));
  hPEN_Black=CreatePen(PS_SOLID,1,RGB(15,15,15));
  hPEN_NULL=CreatePen(PS_NULL,1,RGB(0,0,0));
  hPEN_Cyan=CreatePen(PS_SOLID,1,RGB(255,100,100));
  hPEN_BYellow=CreatePen(PS_SOLID,1,RGB(0,250,0));
#else // __linux__
  hPEN_White=new QPen(QColor(250,250,250),0); // CreatePen (PS_SOLID, 1, ...): width 0 is QPainter's cosmetic 1-pixel pen
  hPEN_Gray=new QPen(QColor(100,100,100),0);
  hPEN_Black=new QPen(QColor(15,15,15),0);
  hPEN_NULL=new QPen(Qt::NoPen); // PS_NULL
  hPEN_Cyan=new QPen(QColor(255,100,100),0);
  hPEN_BYellow=new QPen(QColor(0,250,0),0);
#endif // __linux__
 // hPEN_LGray=CreatePen(PS_SOLID,1,RGB(180,180,160));
#ifndef __linux__
   hPEN_LGray=CreatePen(PS_SOLID,1,RGB(145,49,49));
#else // __linux__
   hPEN_LGray=new QPen(QColor(145,49,49),0);
#endif // __linux__

#ifndef __linux__
  hBRUSH_Brown=CreateSolidBrush(RGB(10,10,10));
  hBRUSH_Sky=CreateSolidBrush(RGB(230,230,230));
  hBRUSH_Yellow=CreateSolidBrush(RGB(16,8,8));
  hBRUSH_BYellow=CreateSolidBrush(RGB(105,100,45));
  hBRUSH_Black=CreateSolidBrush(RGB(15,15,15));
  hBRUSH_TotalBlack=CreateSolidBrush(RGB(0,0,0));
  hBRUSH_FYellow=CreateSolidBrush(RGB(0,50,0));
  hBRUSH_Red=CreateSolidBrush(RGB(49,74,41));
  hBRUSH_Green=CreateSolidBrush(RGB(0,255,0));
  hBRUSH_White=CreateSolidBrush(RGB(255,255,255));
  hBRUSH_StrpWht=CreateHatchBrush(HS_BDIAGONAL,RGB(250,250,250));
  hBRUSH_Background=CreateSolidBrush(RGB(145,48,48));
  hBRUSH_LBkg=CreateSolidBrush(RGB(40,54,59));
#else // __linux__
  hBRUSH_Brown=new QBrush(QColor(10,10,10)); // CreateSolidBrush
  hBRUSH_Sky=new QBrush(QColor(230,230,230));
  hBRUSH_Yellow=new QBrush(QColor(16,8,8));
  hBRUSH_BYellow=new QBrush(QColor(105,100,45));
  hBRUSH_Black=new QBrush(QColor(15,15,15));
  hBRUSH_TotalBlack=new QBrush(QColor(0,0,0));
  hBRUSH_FYellow=new QBrush(QColor(0,50,0));
  hBRUSH_Red=new QBrush(QColor(49,74,41));
  hBRUSH_Green=new QBrush(QColor(0,255,0));
  hBRUSH_White=new QBrush(QColor(255,255,255));
  hBRUSH_StrpWht=new QBrush(QColor(250,250,250),Qt::BDiagPattern); // CreateHatchBrush (HS_BDIAGONAL)
  hBRUSH_Background=new QBrush(QColor(145,48,48));
  hBRUSH_LBkg=new QBrush(QColor(40,54,59));
#endif // __linux__
  //hBRUSH_Gray=CreateSolidBrush(RGB(180,180,160));
#ifndef __linux__
  hBRUSH_Gray=CreateSolidBrush(RGB(145,49,49));
  hFNT_Panel=CreateFont(12,0,0,0,FW_NORMAL,0,0,0,ANSI_CHARSET,OUT_RASTER_PRECIS,
			 CLIP_DEFAULT_PRECIS,PROOF_QUALITY,DEFAULT_PITCH,"Arial");
  hBITMAP_ADI=LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP1));
  hClockSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP1)));
  hSwitchSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP2)));
  hHgaugeSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP3)));
  hEgaugeSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP4)));
  hRotarySRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP5)));
  hTbSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP6)));
  hCbSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP7)));
  hSliderSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP8)));
  hCwSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP9)));
  hMFDSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP10)));
  hDockBSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP14)));
  hDockSW1SRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP16)));
  hDockDlSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP17)));
  hDockSW2SRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP18)));
  hNavSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP19)));
  hRadarSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP20)));
  hRadBkSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP21)));
  hRadSrfSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP22)));
  hFuelSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP23)));
  hVrotSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP26)));
  hVrotBkSRF=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP27)));
  hADIBorder=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP28)));
  hFront_Panel_SRF[1]=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP11)));
  hFront_Panel_SRF[2]=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP12)));
  hFront_Panel_SRF[3]=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP13)));
  hFront_Panel_SRF[4]=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP15)));
  hFront_Panel_SRF[5]=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP24)));
  hFront_Panel_SRF[6]=oapiCreateSurface (LoadBitmap(hModule,MAKEINTRESOURCE(IDB_BITMAP25)));
#else // __linux__
  hBRUSH_Gray=new QBrush(QColor(145,49,49));
  hFNT_Panel=new QFont("Arial"); // CreateFont (12, 0, 0, 0, FW_NORMAL, ..., "Arial")
  hFNT_Panel->setPixelSize(12);
  hBITMAP_ADI=oapiLoadResImage(hModule,IDB_BITMAP1); // LoadBitmap
  hClockSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP1));
  hSwitchSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP2));
  hHgaugeSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP3));
  hEgaugeSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP4));
  hRotarySRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP5));
  hTbSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP6));
  hCbSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP7));
  hSliderSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP8));
  hCwSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP9));
  hMFDSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP10));
  hDockBSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP14));
  hDockSW1SRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP16));
  hDockDlSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP17));
  hDockSW2SRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP18));
  hNavSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP19));
  hRadarSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP20));
  hRadBkSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP21));
  hRadSrfSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP22));
  hFuelSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP23));
  hVrotSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP26));
  hVrotBkSRF=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP27));
  hADIBorder=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP28));
  hFront_Panel_SRF[1]=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP11));
  hFront_Panel_SRF[2]=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP12));
  hFront_Panel_SRF[3]=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP13));
  hFront_Panel_SRF[4]=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP15));
  hFront_Panel_SRF[5]=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP24));
  hFront_Panel_SRF[6]=oapiCreateSurface (oapiLoadResImage(hModule,IDB_BITMAP25));
#endif // __linux__
  Panel_Resources_Loaded=1;
 }
	

void PANEL_ReleaseGDIResources()
#ifndef __linux__
{  DeleteObject(hPEN_White);
   DeleteObject(hPEN_Gray);
   DeleteObject(hPEN_Black);
   DeleteObject(hPEN_NULL);
   DeleteObject(hPEN_Cyan);
   DeleteObject(hPEN_BYellow);
   DeleteObject(hPEN_LGray);

   DeleteObject(hFNT_Panel);
   DeleteObject(hBRUSH_Brown);
   DeleteObject(hBRUSH_Sky);
   DeleteObject(hBRUSH_Yellow);
   DeleteObject(hBRUSH_BYellow);
   DeleteObject(hBRUSH_Black);
    DeleteObject(hBRUSH_TotalBlack);
   DeleteObject(hBRUSH_Red);
   DeleteObject(hBRUSH_Green);
   DeleteObject(hBRUSH_White);
   DeleteObject(hBRUSH_StrpWht);
   DeleteObject(hBRUSH_Background);
   DeleteObject(hBRUSH_LBkg);
   DeleteObject(hBRUSH_FYellow);
   DeleteObject(hBRUSH_Gray);
   DeleteObject(hBITMAP_ADI);
#else // __linux__
{  delete hPEN_White; // DeleteObject
   delete hPEN_Gray;
   delete hPEN_Black;
   delete hPEN_NULL;
   delete hPEN_Cyan;
   delete hPEN_BYellow;
   delete hPEN_LGray;

   delete hFNT_Panel;
   delete hBRUSH_Brown;
   delete hBRUSH_Sky;
   delete hBRUSH_Yellow;
   delete hBRUSH_BYellow;
   delete hBRUSH_Black;
    delete hBRUSH_TotalBlack;
   delete hBRUSH_Red;
   delete hBRUSH_Green;
   delete hBRUSH_White;
   delete hBRUSH_StrpWht;
   delete hBRUSH_Background;
   delete hBRUSH_LBkg;
   delete hBRUSH_FYellow;
   delete hBRUSH_Gray;
   delete hBITMAP_ADI;
#endif // __linux__

   oapiDestroySurface(hClockSRF);	
   oapiDestroySurface(hSwitchSRF);
   oapiDestroySurface(hHgaugeSRF);
   oapiDestroySurface(hEgaugeSRF);
   oapiDestroySurface(hRotarySRF);
   oapiDestroySurface(hTbSRF);
   oapiDestroySurface(hCbSRF);
   oapiDestroySurface(hSliderSRF);
   oapiDestroySurface(hCwSRF);
   oapiDestroySurface(hMFDSRF);
   oapiDestroySurface(hDockBSRF);
   oapiDestroySurface(hDockSW1SRF);
   oapiDestroySurface(hDockDlSRF);
   oapiDestroySurface(hDockSW2SRF);
   oapiDestroySurface(hNavSRF);
   oapiDestroySurface(hRadarSRF);
   oapiDestroySurface(hRadBkSRF);
   oapiDestroySurface(hRadSrfSRF);
   oapiDestroySurface(hFuelSRF);
   oapiDestroySurface(hADIBorder);
   oapiDestroySurface(hFront_Panel_SRF[1]);
   oapiDestroySurface(hFront_Panel_SRF[2]);
   oapiDestroySurface(hFront_Panel_SRF[3]);
   oapiDestroySurface(hFront_Panel_SRF[4]);
   oapiDestroySurface(hFront_Panel_SRF[5]);
   oapiDestroySurface(hFront_Panel_SRF[6]);
Panel_Resources_Loaded=0;
}



