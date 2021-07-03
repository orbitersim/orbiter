Microsoft(R) DirectX(R) 7.0a Software Development Kit

__________________________________________________________________

目次 :

- インストール
- 更新されたSDK 
- CDのレイアウト
- 新機能
- 既知の問題
- コンパイラ サポート

__________________________________________________________________


インストール :

DirectX 7.0a をインストールする前に、DirectX SDK の前のバージョンをアンインストー
ルしてください(次のアンインストール参照)。
ディレクトリ構造とファイル名が大きく変更されています。

全ての DirectX 7.0a SDK と/または ランタイムをインストールするためには、メイン 
メニューから「Install DirectX Foundation 7.0a SDK」を選択するか、CDの \DXF 
ディレクトリにある setup.exe を実行してください。
すでにある MSDN Platform SDK 上にインストールする場合は、この readme の『更新
されたSDK』の項目をご覧下さい。

アンインストール :

アンインストールするには、コントロールパネルの「アプリケーションの追加と削除」
を使用して、DirectX Foundation SDK のこのバージョンあるいは以前のバージョンを
アンインストールしてください。
__________________________________________________________________


更新されたSDK 

本バージョンの DirectX SDK は更に Visual Basic 開発者へのサポートを追加して
います。C のサンプルコード、あるいは Visual Basic のサンプルコード、
あるいは両方のサンプルコードのインストールを選択する事ができます。
インストール時間と必要なディスクスペースを最小にするには、「Custom」
インストールを使ってください。

先の DX6.1 リリースと同様、MSDN プラットフォーム SDK とより良く統合するように
しました。もし以前に MSDN プラットフォーム SDK をインストールしてあれば、
DirectX 7a Foundation SDK はそれを検出し、そのディレクトリにインストールされる
でしょう。そうでない場合は、SDKをどこにインストールするかを選択できます。

__________________________________________________________________


CD のレイアウト

以下に、CDの \DXF ディレクトリ内のディレクトリの簡単な説明を記します。インストー
ル中のオプションを選択する事により、これらのうちのいくつかのディレクトリが、ハード
ディスクにインストールされます。

\Bin
    ハイレベルな DirectX アプリケーションとツール。
  「Utilitys」をインストールすると、全てスタートメニューからアクセス可能です。

\Debug
      DirectX 7.0a DLLのデバッグ バージョン.

\Doc
    DirectX APIのドキュメントが入っています。 最新のDX 7のドキュメントは
    HTMLHelp で最も良く閲覧できます。HTMLHelp ビューアは Internet Explorer 5.0
    (IE5) が必要になります。これは Windows95 へインストールした場合にのみ必要で
す。
    Windows98 と Windows2000 には必要な IE コンポーネントが既に含まれています。
    IE5 をインストールするには http://wwww.microsoft.com/japan/ie を参照して
    ください。もし IE のインストールを選択しなかった場合でも、
    DXF\docs\directX7\word\ にあるWord ファイルにアクセスすることが出来ます。

\Essentls
    DMusProd (Direct Music Producer)
       - Direct Music Producer は DirectMusic のオーサリング ツールです。
         これを使えば DLS のコンシステント サウンド パフォーマンスに合わせて、
         インタラクティブに複数の DirectMusic リソースを使う事ができます。 
         Music Producer セットアップ プログラムと全てのファイルはここにあります。
 
\Extras
    DDSPlugin
      - ここには DDS ファイルフォーマットを使ったDXTn 圧縮テクスチャを
        インポート・エクスポートする Photoshop プラグインが入っています。
    DMusic
      - このディレクトリにはloader.zip ファイルがひとつだけ入っています。
        このファイルにはアプリケーションが　DirectMusic ローダ自身を書く
        方法を示すサンプルコードが入っています。ここに含まれているのは学習用で、
　　　　   参照としてのみ使われるべきです。
　　　　DSound
      - このディレクトリには mulchaud.doc が一つだけ入っています。これはマルチ 
        チャンネルと高ビット深度のオーディオ データ ファイルについての新しいウェー
        ブ フォーマット構造について記述しています。以前のウェーブフォーマット構造
        は8ビットと16ビットの整数フォーマットに制限されていました。更にチャンネル
        の数が2以上の時には、追加チャンネルの意味は指定できませんでした。このス
        ペックのフォーマットを使うと、アプリケーションは2チャンネル以上で8ビット
        16ビット整数に制限されないウェーブファイルを作成したり利用したりできるで
        しょう。チャンネル数が2以上の場合も、正しいアウトプットとスピーカ出力が指
        定できます。
    DXSymbols
      - デバッグ時に開発者に役立つシンボルファイルを追加しました。
        Win9x と　Win2K 用(".pcb"と".dbg")のリテール版、デバッグ版両方があります。
    VTune
      - VTune(TM) Performance Analyzer, version 4.0, from Intel(R).  トライアル
        版の VTune Performance Analyzer がこのディレクトリに含まれています。
        VTune Performance Analyzer は、Microsoft Windows 95, Windows 98 および
        Windows NT システム用の、統合チューニング環境を提供します。VTune
        Performance Analyzer は、Intel アーキテクチャに特化したソフトウェアのパ
        フォーマンス データを、システム全体から、特定のモジュール、関数、または、
        インストラクションに渡って収集、解析します。VTune coach は、Intel アーキ
        テクチャ上での、パフォーマンス向上のための推奨されるコードの変更方法を
        提供します。本ソフトウェアに関する詳しい情報、および Intel のソフトウェア
        パフォーマンス製品関連の情報は、http://developer.intel.com/vtune を参照し
        てください。
        注記：Microsoft は、この製品の動作、安定性および性能に関しての保証は致し
        ません。
    Win98 PIII DBG
      - このディレクトリにあるファイル (VMCPD.VXD) は Windows98 Pentium III 
        マシン上で DX6.1 あるいは DX7 の D3D アプリケーションをデバッグするために
        必要です。これは Windows98 だけの問題です(Win98 SE では問題ない)。
        詳細はこのディレクトリの readme.txt をご覧ください。
  
\Include
    DirectDraw, Direct3D, DirectSound, DirectInput, DirectPlay, DirectMusicの
    インクルード ファイルが入っています。

\Lib
    DirectDraw, Direct3D, DirectSound, DirectInput, DirectPlayのライブラリ
    ファイルが入っています。
    *.LIB : COFF libraries (Microsoft Visual C++ 2.0 またはそれ以降)
    Borland 11.0 版のライブラリを含むサブディレクトリもあります。

\License
    DirectX SDK と End User License Agreements そして、
    Redistributable License Agreementのテキスト版

\Redist
    DirectX 7.0a DLLの再頒布可能モジュール

\Samples
    全てのサンプルコードとバイナリが入っています。詳細は各ディレクトリに含まれる
    readme.txt をご覧下さい。大部分のサンプルは、スタートメニューからアクセス可能
    です。
	
__________________________________________________________________


新機能 :

DirectX for Visual Basic
========================

DirectX はこれまでゲーム コンテンツを開発する C と C++ 開発者の範囲でに使われて
きました。Visual Basic 言語サポートの提供により、DirectX API は同じような
あるいは非常に異なった種類のアプリケーションをかく新しい種類の開発者たちに
オープンになりました。このプロジェクトのゴールはC開発者と同じハイ パフォーマンス
のマルチメディア機能へのアクセスをシンプルに提供する事です。結果的に 
Visual Basic サンプルコードとヘルパー コントロールを使って、Visual Basic 
プログラマにとって DirectX をアクセス容易で理解しやすくなるよう SDKを強化する
事もできました。

DirectX for Visual Basic の中核となるのは DirectX ランタイムと Visual Basic の
オブジェクトを接合する1つの DLL です。この DLL は DirectX の機能を Visula Basic
になじみのタイプを使ってエクスポートし、DirectX オブジェクトのいくつかから
Visual Basic 言語開発者を隔離します。最大限のフレキシビリティとスピードを
提供するために、オブジェクト モデルはCのそれを反映します。

DirectX ドキュメントを参照してください、これは複数言語(C と VB)に対応しています。
SDK の Visual Basic サンプルコードには追加の情報が含まれています。


Direct3DIM
==========

- DirectX 7.0 の Direct3D では多くの新しい機能と並んで、プログラムを単純化し
  より高速なパフォーマンスをもたらす新しいインターフェイス群を紹介します。
  Direct3D インターフェイスの整理統合によりプログラミング モデルは単純化され
  ました。マテリアル、ビューポート、ライトの情報は異なったオブジェクトではなく
  デバイス ステートの一部となりました。テクスチャ オブジェクトを分離する必要は
  なくなり、テクスチャは単に DirectDraw サーフェイスとなりました。

新しいインターフェイスに含まれことで可能となった機能 :
   - ハードウェア トランスフォーム と ライティング
   - 頂点 ブレンディング
   - 任意のクリップ プレーン
   - 立方体(Cube)環境マッピング
   - テクスチャ トランスフォームと射影テクスチャ
   - 強化されたテクスチャ マネージメント
   - ステート ブロック

D3DX
====
D3DXは COM ベースのヘルパーAPIで、DirectX 7.0 の Direct3D と DirectDraw の上
に位置します。これは、いくつかのヘッダーファイルとライブラリ（リテール版は、
d3dx.lib でありデバッグ版は、d3dxd.lib です）によって構成され、アプリケーション
は、スタティックにリンクできます。Direct3DX ユティリティライブラリは、3D グラ
フィックス開発者が遭遇する、共通のタスクを簡素化するためのヘルパー関数を提供し
ます。

ユティリティライブラリは、Direct3D Immediate Mode を迅速に、かつ効率的に使用し
たい開発者のためにデザインされました。ユティリティライブラリは、よく繰り返され
るタスクを取り除くことを目的とした、ショートカットのツールキットで、これにより、
開発者は、アプリケーションのコアやゲームの開発に専念することができます。

Direct3DX ユティリティライブラリは、デバイス構成の列挙、デバイスのセットアップ、
フルスクリーンまたはウィンドウモードでの一貫した実行、リサイズ操作の実行、ベク
タと行列演算、そしてイメージファイルのロードとテクスチャ作成の簡素化などのヘル
パー関数を提供します。さらに、単純な形状、スプライト、そしてキューブマップの描
画関数も提供します。Direct3DXは、シーンの階層化や X ファイルのサポートは提供し
ません。

このユティリティライブラリは、最新の DirectX 7.0 があなたのシステムにインストー
ルされている必要があります。Direct3DX ユティリティライブラリは、複数のモニタの
構成をサポートします。Direct3DX のオブジェクトは、マルチスレッドに対応していま
せん。マルチスレッドのアプリケーションは、Direct3DX オブジェクトにアクセスする
ためには、自分でロックと同期プリミティブを追加しなければなりません。

DirectDraw
==========

- DX7 には新しい DirectDRAW インターフェイスがあります。これは IdirectDraw7 
  です。以前の DirectDraw インターフェイスと Direct3D インターフェイスとの関係は
  以下のようになっていますので注意してください。

  変更は DirectX7 Direct3D インターフェイスを以前のバージョンのインターフェイス
  から切り離すために行われました。DirectDraw オブジェクトに2つの新しいクラスがあ
  ります、それはDirectX7 のオブジェクトである DirectDrawCreateEx で作成されるもの
  と、DirectX7 以前の古いオブジェクトである DirectDrawCreate によって作成される
  ものです。DX7 も DX7以前のオブジェクトも(IDirectDraw*7を含む)全てのインター
  フェイスをサポートしますが、DX7 DirectDraw オブジェクトだけは DX7 D3D インター
  フェイス IDirect3D7 をサポートします。更に Direc3D7 インターフェイス メソッドは
  IDirectDraw*7 インターフェイスを排他的に使用します。

  結果的に、これまでの Direct3D インターフェイスを使う場合にはバージョン7の 
  DirectDraw を使うべきではありません、DirectX 7 Direct3D インターフェイスを使う
  場合にアプリケーションはDirectDrawCreateEx から取得した DirectX 7 DirectDraw 
  インターフェイスを使うべきです。

　実際例として、DirectX 7 DirectDraw オブジェクトを作成するにはアプリケーション
  は DirectDrawCreateEx を使うべきです、そして IDirectDraw7 ポインタをその関数から
  受け取ります。アプリケーションは次に QueryInterface を使って IDirect3D7 インター
  フェイスを取得します、そして IDirectDraw7::CreateSurface を使ってサーフェイスを
  作成し IDirectDrawSurface7 インターフェイスを取得します。この DirectDraw オブ
  ジェクトは全てのインターフェイスレベルをサポートしているので、例えばこのサー
  フェイスを DirectShow で使うためにIDirectDrawSurface7 インターフェイスから 
  IDirectDrawSurface4 に関する QueryInterface を順に行うことは可能です。
  DirectX 7 DirectDraw オブジェクトは IDirect3D4 とそれ以前のインターフェイスを
  サポートしていないので、たとえこの DirectDraw オブジェクトから(QueryInterface
  や他の方法で)以前の DirectDraw インターフェイスが取得できたとしても、IDirect3D4 
  インターフェイスを取得するために QueryInterface を使うことはできません。

  更に、以前のインターフェイスではテクスチャは Idirect3Dtexture インターフェイス
  を通して作成されました。DX7 ではこのインターフェイスはなくなりました。代わりに  
  Idirect3Ddevice7 上に GetTexture、SetTexture、Load メソッドが 
  IdirectDrawSurface7 のポインタを提供します。IdirectDrawSurface7 
  インターフェイスは次の関連メソッドを含んでいます。
   - SetPriority と GetPriority
   - SetLOD と GetLOD
 
  これらのインターフェイスの変更に加えて、DirectDraw は2つの新しい機能をサポート
  します。
   ステレオ - これにはステレオ フリップとリフレッシュレート列挙のサポートを含み
     ます。
   立方体(Cube)環境マッピング - DirectDraw はこの複雑なサーフェイス構造の
     自動生成をサポートします。従ってアプリケーションはサーフェイス内のそれぞれの
     面を、1つのDirectDraw サーフェイスとして取り扱う事ができます。それらの
     サーフェイスに対してLock、GetDC、BLT image が使え、それらをレンダリング 
     ターゲットとして使用する事もできます。


DInput
========================
DInput にはエクスクルーシブモードでキーボードデバイスを取得することで、Windows 
Key を使えなくする機能が追加されました。


DirectMusic
===========

- このバージョンの DirectMusic は Win 98SE と Win 2000 プラットフォームでの 
  DLS ハードウェア アクセラレーションをサポートします。また DirectMusicでは 
  DLS2 をサポートする DirectMusic シンセサイザ上で DLS2 ファイルを再生する事が
  できるようになりました（ このリリースでは DirectMusic ソフトウェア 
  シンセサイザは DLS2 再生をサポートしない事に注意してください）。更に DirectMusic
  は "RMID" フォーマットをサポートします。このフォーマットはDLSの集合と組み合
  わされた MIDI ファイルを1つのファイルにまとめます。


DirectMusic Producer
========================
このバージョンの Producer には DirectMusic コンテンツのオーサリングと試演の両方
を援助する新しいフィーチャーが多く追加されました、それはセカンダリ セグメント 
ツールバーの強化や、シーケンサの出力を MSSynth にマップする Echo MIDI In フィー
チャー、Wave と MIDI のエクスポートです。シーケンストラックのフル表示や編集のよ
うな多くの使い勝手の改良も行われました、それはハイブリッド ノーテーション、
出力ステータスバーへの pchannel 名の表示、複数インストゥルメント ミキシング、プロ
パティページでのウェーブ ループ ポイントの編集機能です。

DirectPlay
==========

- DirectPlay ではリップル ラウンチングのサポートが追加されました。
  リップル ラウンチングはあるアプリケーションが他のものを使って準備をするために
  定義されました。これは暗号による保護のために用いられる1つのテクニックです。
  DPlay はアプリケーション インストール時にレジストリに登録されるコマンドライン
  パラメータを通してリップル ラウンチングを取り扱います。これはSDK ヘルプページ
  に記載されています。

DirectSound
===========

- DirectSound の本バージョンではビルトイン ハードウェア ボイス マネージメントを
  提供します。この機能によってアプリケーションが DirectSound バッファを作成して
  も、そのバッファが再生されるまではハードウェアにもソフトウェアにもバッファは
  確保されません。これによりオーディオ ハードウェア バッファを最も効率的に利用
  できます。

- DirectSound 7 ではまた選択可能な　3D HEL を提供します。これによりアプリケー
  ションは 3D サウンドを処理するとき、バッファごとにソフトウェアアルゴリズムを
  選択することができます。3つのアルゴリズムが利用可能です:　NO_VIRTUALIZATION　
  これは単純な左右パンやドップラーやボリューム変更に XYZ 座標をマップします。
  NO_VIRTUALIZATION これは普通の非3Dバッファとして同じ量のCPUを使用します。
  HRTF_LIGHT はシンプルで均等なCPU効率アルゴリズムを提供し、HRTG_FULL は幾分
  CPUをより使用するが非常に説得力ある 3D オーディオエフェクトを提供します。
  HRTF_LIGHT と　HRTF_FULL は Wondws98 Second Edition のみで有効だ
  ということに注意してください。

- DirectSound ではまたWindows 98 Second Edition と Windows 2000 で 
  WDM オーディオドライバのシステム上で任意のフォーマットのマルチ チャネル バッ
  ファを作成することができます。

- DirectSound のデバッグ版を使うとき、DirectSound バッファはある　static-like 
  ノイズで初期化されます。これによってあるバッファを再生しようとしたとき正しい
  オーディオデータが書き込まれているかを確認できるので、アプリケーションは 
  DiretSound のデバッグができます。DirectSound バッファを再生したとき、いっせい
  にノイズが聞こえたら、ほとんどはこの場合です。

__________________________________________________________________


既知の問題点

ドキュメント
===========

- DirectX SDK ドキュメントが使用する HTML ヘルプエンジンは選択によって複数のト
  ピックを印刷できますが、次のような制限があります。
    1) (フォントやアラインメントの)あるフォーマットを失います
    2) 言語フィルタは動作しません

- ドキュメントの IDirect3DVertexBuffer7::ProcessVerticesStrided の5番目のパラメー
  タの記述に誤りがあります。正しいプロトタイプとパラメータの記述は、次のようにな
  ります。

  HRESULT ProcessVerticesStrided( 
    DWORD dwVertexOp, 
    DWORD dwDestIndex, 
    DWORD dwCount, 
    LPD3DDRAWPRIMITIVESTRIDEDDATA lpVertexArray, 
    DWORD dwVertexTypeDesc, 
    LPDIRECT3DDEVICE7 lpD3DDevice, 
    DWORD dwFlags
  );

  dwVertexTypeDesc
    A combination of flexible vertex format flags that describes the
    vertex format.  

DirectDraw/Direct3DIM
=====================

- 3dfx Voodoo2 と Windows 2000 :
  DirectDraw for Windows 2000 は 3dfx Voodoo2 のパススルー 機能を完全にはサポート
  していません。対応策としては替わりにパススルー スタイルのデバイスとしてセカンダ
  リ(マルチモニター) デバイスを DirectDrawEnumerate を使ってリストする
  事です。
  つまり Voodoo2 を使ったソフトウェア開発時に、開発者は Voodoo2 をセカンダリ 
  マルチモニター デバイスとして設定するべきです。その後で対応策として DirectDraw
  を有功にします。次の DWARD レジストリ エントリーを0以外の値に設定する事で
  有効にできます、
  HKLM\Software\Microsoft\DirectDraw\EnumerateAttachedSecondaries 。

-以前の D3DLIGHT 構造体を使用する際の既知の問題 :  
1) D3DLIGHTSTATE_COLORVERTEX ライト ステートはサポートされていないわけでは
   ありませんが、振る舞いは正しくありません。スペキュラの計算はサポートされており、
   ディフューズの計算はサポートされていません。
2) D3DLIGHT 構造体を使って光源を生成した場合、点光源とスポット光源のスペキュラの
   計算は正しくありません。減衰ファクタは光源と頂点との距離で除算されます。

- Win98 Pentium III マシン上でデバッグ時の例外がマスクされません :
  Win98 でこれは「Unhandled exception」あるいは「Illegal Instructions」を生成
  します。この問題を修正するには、ファイルを1つコピーする必要があります。
  詳細の情報は「\DXF\Extras\Win98 PIII DBG」ディレクトリをご覧ください。

- パフォーマンスの理由で、新しい Idirect3Ddevice7 インターフェイスをサポート
  しているレンダリング デバイスはFPU を単精度にセットします。この振る舞いは 
  Idirect3Ddevice3 のようなこれまでのインターフェイスをエクスポーズする
  デバイスのそれとは相反します。詳細の情報は Direct3D Immediate Mode SDK 
  ドキュメントの「DirectDraw Cooperative Levels and FPU」をお読みください。

- Direct3D で STL を使う時、STL ヘッダは d3d.h の後でインクルードしなければ
  なりません。

- カラーバッファとZバッファが同じビット数でないとレンダリング ターゲットが
  作成できないボードがあります。次のような制限があります:
      16 ビット カラーバッファには 16ビット Z バッファが必要 ( ステンシル無し )
      32 ビット カラーバッファには 32 ビット Z バッファ 
      ( うち 8 ビットは ステンシルになり得る )
  もしこの条件を満たさないと、 CreateDevice() あるいは SetTendertarget() が
  成功しません。この場合 GetDeviceIdentifier() を使って問題を避けてください。


DirectPlay
==========

- DirectPlay4A::EnumConnections() API はシステム上の有効なサービスプロバイダに
  ついて必ずシングルバイト キャラクタ の文字列を返します。アプリケーション全体を
  Unicode に変更せずローカライズしたマルチバイトの文字列を得るには、次の手順を
  行います。

      a) IDirectPlay4 インターフェイス(Unicode)で、QueryInterface
      b) IDirectPlay4::EnumConnections をコール - これによって Unicode 文字列を
         取得
      c) それぞれのストリングを Windows のWideCharacterToMultiByte() 関数を
         使って ANSI に変換
      d) IDirectPlay4 インターフェイスをリリース
      e) IDirectPlay4A インターフェイスを以降のアプリケーションでは使用し続ける。

  もう一つの方法として、IDirectPlay3A インターフェイスで QueryInterface をする
  事もできます。これも ローカライズされたマルチバイト キャラクタ 文字列を返し、
  ステップ c) で列挙できます。

- US Roboticsモデムの最新のドライバはセッションをホストする際に問題を引き起こす
  ことがあります。これはドライバのバグです。

- モデム セッションを列挙している際に、電話回線を切断するとロックアップします。

- DirectPlay でプロトコルフラグを使用したとき、Windows 98SE の機能である ICS
  (Internet Connecton Sharing)では動作しません。Windows 2000 では、このような
  問題は、発生しません。

Direct3DRM for Visual Basic
===========================

- D3DRM コールバック。Frame3.AddMoveCallback  や Object.AddDestroyCallback 
  のようなコールバックはアプリケーションを終了させる前にそれを削除してクリーン
  アップしなければなりません。例えば、Frame3.DeleteMoveCallback  をコールして
  削除しなければなりません。コールバックの削除を行わないと、システム クラッシュ
  を起こす場合があります。


サンプル 
=======

- D3DIM サンプルは、DX7 ソフトウェア ラスタライザ上で動作可能です。しかしながら
  レジストリキーは、デフォルトで設定されておりません。
  正しいレジストリ設定は、DXF\samples\Multimedia\D3DIM\bin ディレクトリの
  ファイルを使って行うことができます。ソフトウェア ラスタライザのレジストリキー
  の設定を行わないと、バンプマップなどのDX 7特有のサンプルを最初に起動したときに
  それを可能にするようメッセージが出るでしょう。
  これは、ソフトウェア ラスタライザなので、非常に低いフレームレートであることを
  ご了承下さい。

- デスクトップの解像度と、システムのビデオメモリーの使用可能な量によりますが、
  サンプル テクスチャに対して十分なビデオメモリーが無い場合があります。
  この場合 D3DIM サンプルは動作しますが、テクスチャを表示できません
  (サーフェイスは白になるでしょう)。テクスチャを表示させるにはデスクトップの
  解像度をより低くするか、ディスプレイカードにより多くのメモリーを搭載して
  ください。

- DMHook DLL ( DMShell のヘルパー DLL ) は Watcom や Borland のコンパイラでは
  コンパイルできません。Watcom と Borland ユーザーはSDK サンプルバイナリで
  提供される、既に作成されたDMHook.dll をお使いください。

- 特定のハードウェアやドライバで、様々な問題があります。貴方がサンプルや
  ドライバに起因すると思える問題はぜひお知らせ下さい。 reference rasterizer 
  の使用が判断の助けになります。

- 日本語版の Windows を使っている場合、サンプル プログラムのダイアログ ボックス
  テキストで漢字が表示できないかもしれません。漢字を表示させるには、.RC ファイル
  を編集してダイアログ フォントを日本語に変更してください。

- サンプルの詳細な情報は <drive>:\(sdk_path)\samples\multimedia の 
  dxreadme.txt を参照してください。


その他
======

- DirectX 7a SDK をインストールした後で、Windows 2000 にアップグレードしたマシン
  では、DirectX コントロールパネルが正しくアップグレードされません。しかし　
  Windows 2000 にアップグレードした後で DirectX 7a SDK を再インストールすれば、
  DirectX コントロールパネルを追加できます。


__________________________________________________________________


コンパイラ サポート

C/C++
=====

DX7 サンプルは VC++ 4.2 以降で動作します。VCプロジェクト ファイル（
VC 4.2 がサポートする .mdp）とVC 用の makefile が含まれています。

以前のリリースと同様、Watcom V11.0 と Borland CBuilder 3 ユーザーのサポート
を行っております。ただし、今回のリリースでは、下記のサンプルを、Watcom と
Borland でコンパイルすることはできません:

Watcom: 1) D3DIM Screensaver サンプル;  2) DMusic サンプルの DMHook.

Borland: 1) DMusicの DLL DMhook;  2) DDrawの DXTex;  3) D3DIM Screensaver; 
　　　4) D3DIMの VideoTex.

コンパイラをインストールする際は、MFC サポートをインストールすることを強く推奨し
ます。多くのサンプルは、MFC を使用しています。このサポートがないと、いくつかのサ
ンプルは、コンパイルできません。

また、Watcom　や　Borland でコンパイルするとき、コンパイルしようとするサンプルの
ディレクトリ内に居なければなりません。全てのサンプルのコンパイルを提供する
dxall.mak ファイルは Visual C++ でのみ動作します。


ビルド環境の設定には、CDRom:\DXF\samples\Multimedia\DXReadme.TXT をご覧下さい。
この README に次のようなビルド方法のセクションがあります、Note for VC++ Users、
Note for Watcom Users、Note for Borland Users。SDK はこの readme ファイルを 
<drive:>\(sdk_path)\samples\multimedia にインストールします。

Visual Basic
============

DirectX 7  SDK は Microsoft Visual Basic v5.0 サービスパック3(SP3) 以降をサポート
します。Visual basic のそれ以前のバージョンのシステムに DirectX 7 SDK をインストー
ルすると、システムファイルをVisualBasic 6.0 のファイルに上書きしてしまいます。

もしあなたが、Windows 95 のユーザーに対して、DirectX Events をしようした Visual
Basic アプリケーションを提供するならば、あなたのインストーラは、DCOM95 を含むこ
とが重要です。もし、これを行わないと、Windows 95 上で、アプリケーションを終了する
際、ページフォルトが発生することがあります。DCOM コンポーネントは、Internet
Explorer 4 と Visual Basic に含まれていますが、エンドユーザーの環境を知ることは
できません。Windows 95 ユーザーに対してこのような問題を回避させるには、DCOM を
含めることです。DCOM95 は、VB5 の CD か、Web サイト http://www.microsoft.com/dcom 
から入手できます。
