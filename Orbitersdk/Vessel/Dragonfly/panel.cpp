
#include "panel.h"
#include <windows.h>
#include < GL\gl.h >
#include <math.h>
#include <stdio.h>
#include "orbitersdk.h"
#include "resource.h"

HFONT hFNT_Panel;
HBRUSH hBRUSH_Black,hBRUSH_Yellow,hBRUSH_BYellow,hBRUSH_Red,hBRUSH_Green,hBRUSH_Background,hBRUSH_LBkg;
HBRUSH hBRUSH_TotalBlack;
HBRUSH hBRUSH_FYellow,hBRUSH_Gray;
HBRUSH hBRUSH_White,hBRUSH_StrpWht;
HBRUSH hBRUSH_Brown,hBRUSH_Sky;
HPEN hPEN_White,hPEN_Gray,hPEN_Black,hPEN_NULL,hPEN_Cyan,hPEN_BYellow;
HPEN hPEN_LGray;
HBITMAP hBITMAP_ADI;
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

int LoadOGLBitmap(char *filename)
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

   l_texture = (byte *) malloc(infoheader.biWidth * infoheader.biHeight * 4);
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


void MoveTo(HDC PANEL_hdc, int x, int y)
{ HPEN oldp;
  oldp=static_cast <HPEN> (SelectObject(PANEL_hdc,hPEN_NULL));
  LineTo(PANEL_hdc,x,y);
  SelectObject(PANEL_hdc,oldp);
};
Panel::Panel()
{
	instruments=NULL;
	screw_num = 0;
};

Panel::~Panel()
{ DeleteDC(hDC3);
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
	hDC2=CreateCompatibleDC(hDC);
	hDC3=CreateCompatibleDC(hDC);
	hBitmap=CreateCompatibleBitmap(hDC,Wdth,Hght);
	HBITMAP hBitmapOld=(HBITMAP)SelectObject(hDC2,hBitmap);
	DeleteObject(hBitmapOld);
	SelectObject(hDC2,hBRUSH_Background);
	Rectangle(hDC2,0,0,Wdth,Hght);
	Panel::NowPutScrews();
	Panel::NowPutTextOnBackground();
	Panel::NowPutCText();
	Panel::NowPutBorders();
	DeleteDC(hDC2);
	oapiReleaseDC(surf,hDC);
	oapiDestroySurface(surf);

	
}
void Panel::NowPutScrews()
{int di=5*sqrt(2.0)/2;
 SelectObject(hDC2,hPEN_White);
 SelectObject(hDC2,hBRUSH_Background);
 int x,y;
	for (int i=0;i<screw_num;i++)
	{  x=Screw_list[i].x;
	   y=Screw_list[i].y;
	   Ellipse(hDC2,x-5,y-5,x+5,y+5);
	   MoveTo(hDC2,x-di,y-di);LineTo(hDC2,x+di,y+di);
	}
};
void Panel::NowPutTextOnBackground()
{  SetBkMode(hDC2,TRANSPARENT); 
 //  SetTextColor(hDC2,RGB(0,147,221));
SetTextColor(hDC2,RGB(255,100,100));
   SetTextAlign(hDC2,TA_CENTER);
   SelectObject(hDC2,hFNT_Panel);
   for (int i=0;i<text_num;i++)
	TextOut(hDC2,Text_list[i].x,Text_list[i].y,Text_list[i].text, sizeof(char)*strlen(Text_list[i].text));
  
}
void Panel::NowPutCText()
{ 
  SetBkMode(hDC2,TRANSPARENT); 
  //SetTextColor(hDC2,RGB(0,147,221));
  SetTextColor(hDC2,RGB(255,100,100));
  SelectObject(hDC2,hPEN_Cyan);
  SetTextAlign(hDC2,TA_CENTER);SetBkColor(hDC2,RGB(145,48,48));
  SetBkMode(hDC2,OPAQUE);
  SelectObject(hDC2,hFNT_Panel);
  for (int i=0;i<ctext_num;i++) {  
  MoveTo (hDC2,CText_list[i].x+CText_list[i].lngth/2, CText_list[i].y+CText_list[i].dir*7+5);LineTo(hDC2,CText_list[i].x+CText_list[i].lngth/2,CText_list[i].y+5);
  LineTo(hDC2,CText_list[i].x-CText_list[i].lngth/2,CText_list[i].y+5);LineTo(hDC2,CText_list[i].x-CText_list[i].lngth/2,CText_list[i].y+CText_list[i].dir*7+5);
 
  TextOut(hDC2,CText_list[i].x,CText_list[i].y,CText_list[i].text, sizeof(char)*strlen(CText_list[i].text));
  }
};
  
void Panel::NowPutBorders()
{ int px,py,nr;
 SetBkMode(hDC2,TRANSPARENT); 
 SelectObject(hDC2,hPEN_Black);
 for (int i=0;i<border_num;i++){
 px=Border_list[i].x;py=Border_list[i].y;
 nr=Border_list[i].num;

 Arc(hDC2,px-5,py-5,px+6,py+6,px,py-5,px-5,py);
 MoveTo(hDC2,px-5,py);LineTo(hDC2,px-5,py+40);py+=40;
 Arc(hDC2,px-5,py-5,px+6,py+6,px-5,py,px,py+5);
 MoveTo(hDC2,px,py+5);LineTo(hDC2,px+45*nr+5,py+5);px+=45*nr+5;
 Arc(hDC2,px-5,py-5,px+6,py+6,px,py+5,px+5,py);
 MoveTo(hDC2,px+5,py);LineTo(hDC2,px+5,py-40);py-=40;
 Arc(hDC2,px-5,py-5,px+6,py+6,px+5,py,px,py-5);
 MoveTo(hDC2,px,py-5);LineTo(hDC2,px-45*nr-5,py-5);
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
	                       oapiWriteScenario_string (scn, "    SW ", cbuf);
						   index=0;
						}
	};
  //end of inst list. dump the rest
  if (index>0)
		{
		int_sv[index]=99;
		sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	    oapiWriteScenario_string (scn, "    SW ", cbuf);
		index=0;
		};
  cbuf[0]=0;
  oapiWriteScenario_string (scn, "  SW END ", cbuf);
  //save the rotary
  runner=instruments;
  index=0;
  while (runner)
  { if ((runner->instance)&&(runner->instance->type==34))
		    int_sv[index++]=((Rotary*)runner->instance)->set;
 		    runner=runner->next;
			//every 5 switches save
			if (index==5) {sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	                       oapiWriteScenario_string (scn, "    ROT ", cbuf);
						   index=0;
						}
	};
  //end of inst list. dump the rest
  if (index>0)
		{
		int_sv[index]=99;
		sprintf (cbuf, "%i %i %i %i %i ",int_sv[0],int_sv[1],int_sv[2],int_sv[3],int_sv[4]);
	    oapiWriteScenario_string (scn, "    ROT ", cbuf);
		index=0;
		};
  cbuf[0]=0;
  oapiWriteScenario_string (scn, "  ROT END ", cbuf);
  runner=instruments;
  //index=0;
  while (runner)
  { if ((runner->instance)&&(runner->instance->type==44))//ADI ball saving
			{
			ADI* adi_p=(ADI*)runner->instance;
			sprintf(cbuf,"%0.4f %0.4f %0.4f %i %i %0.4f %0.4f %0.4f",adi_p->now.x,adi_p->now.y,adi_p->now.z,
															    adi_p->orbital_ecliptic,adi_p->function_mode,
																adi_p->reference.x,adi_p->reference.y,adi_p->reference.z);
			oapiWriteScenario_string (scn, "    ADI ", cbuf);
			};//end of if
    runner=runner->next;
  };//end of while
  cbuf[0]=0;
  oapiWriteScenario_string (scn, "  PAN END ", cbuf);
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

void PANEL_DLLAtach()
{Panel_Resources_Loaded=0;}


void PANEL_InitGDIResources(HINSTANCE hModule)
{ if (Panel_Resources_Loaded) return;
  hPEN_White=CreatePen(PS_SOLID,1,RGB(250,250,250));
  hPEN_Gray=CreatePen(PS_SOLID,1,RGB(100,100,100));
  hPEN_Black=CreatePen(PS_SOLID,1,RGB(15,15,15));
  hPEN_NULL=CreatePen(PS_NULL,1,RGB(0,0,0));
  hPEN_Cyan=CreatePen(PS_SOLID,1,RGB(255,100,100));
  hPEN_BYellow=CreatePen(PS_SOLID,1,RGB(0,250,0));
 // hPEN_LGray=CreatePen(PS_SOLID,1,RGB(180,180,160));
   hPEN_LGray=CreatePen(PS_SOLID,1,RGB(145,49,49));

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
  //hBRUSH_Gray=CreateSolidBrush(RGB(180,180,160));
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
  Panel_Resources_Loaded=1;
 }
	

void PANEL_ReleaseGDIResources()
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



