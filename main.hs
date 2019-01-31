{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE NoDisambiguateRecordFields #-}
{-# LANGUAGE NoRecordWildCards #-}
-- Typeable!
import Yesod
import Yesod.Static
import Yesod.Auth.GoogleEmail2
import Yesod.Auth
import Yesod.Auth.Email
import Text.Hamlet
import Text.Lucius
import Data.Text (Text, pack, unpack, )
import Data.Time.Calendar (toGregorian)
import Data.Time (UTCTime, getCurrentTime, showGregorian, utctDay)
import Data.String
import Data.Typeable (Typeable)
import Database.Persist 
import Database.Persist.Sql (toSqlKey, fromSqlKey, rawSql)
import Database.Persist.Postgresql ( ConnectionPool, SqlBackend, runSqlPool, runMigration,withPostgresqlPool, runSqlPersistMPool)
--import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Typeable (Typeable)
import Yesod.Auth
import Network.HTTP.Types
import Data.Monoid
import Prelude
import GHC.Int(Int64)
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)
import CustomFields.Categoria

import           Data.Maybe               (isJust)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.Text.Lazy.Encoding
import           Database.Persist.TH
import           Network.Mail.Mime
import           Text.Shakespeare.Text    (stext)
import           Control.Monad            (join)

clientId :: Text
clientId = "436684273817-dnf0r1fmts1br6rjvsj8n4kug1sgvjo1.apps.googleusercontent.com"

clientSecret :: Text
clientSecret = "_WSSfx0LULWnGPrpcvjz_mPY"

{- =====================================
    Instancias de tipos(TypeClass,TypeFamily), Rotas, Shares, Type synonyms
======================================== -}
data RomefellerBlog = RomefellerBlog 
                      {connPool :: ConnectionPool
                      ,getStatic :: Static
                      ,httpManager :: Manager         
                      }

data MesLiteral = Janeiro | Fevereiro | Março | Abril | Maio
        |Junho | Julho | Agosto | Setembro | Outubro | Novembro | Dezembro
        | ErroMes
        deriving Show
        
-- data CategoriaField = CategoriaField

staticFiles "static"

mkMessage "RomefellerBlog" "messages" "pt-br"

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkDeleteCascade sqlSettings] [persistLowerCase|
Administrador
    email Text
    UniqueAdministrador email
    deriving Show
Usuario
    email           Text
    senha           Text Maybe
    token           Text Maybe
    verificado      Bool
    nome            Text Maybe
    UniqueUsuario email
    deriving Typeable
    deriving Show
Postagem
    usuarioId UsuarioId
    visivel Bool
    categoria Categoria
--  editcao Maybe UTCTime
    titulo Text
    data UTCTime default=now()
    conteudo Textarea -- Alterar o tipo para textArea
    deriving Show
Curtida
    postagemId PostagemId
    usuarioId  UsuarioId
    UniqueCurtida postagemId usuarioId 
    deriving Show
Comentario
    postagemId PostagemId
    comentarioId ComentarioId Maybe  --(auto-relacionamento)
--  editacao Maybe UTCTIme
    data UTCTime 
    usuarioId UsuarioId 
    conteudo Textarea
    deriving Show

|]
-- Criar o tipo Categoria para substituir a tabela Categoria. É necessário criar um CustomField para ser utilizador no InputForm
mkYesod "RomefellerBlog" [parseRoutes|
/auth AuthR Auth getAuth

 / BlogR GET
!/#PostagemId                   PostagemR GET 
 /postagem/curtir/#PostagemId   PostagemCurtirR PATCH 
 
 /comentario/#PostagemId                                    AdicionarComentarioR POST
!/comentario/#ComentarioId/#UsuarioId/#PostagemId           RemoverComentarioR   GET
!/comentario/editar/#ComentarioId/#UsuarioId/#PostagemId    EditarComentarioR    POST -- rever a implementação desta rota. Criar um novo fomulário com tipo Tupla (foda-se o typeSafe)
 

 /adm                              AdminR           GET
!/adm/#PostagemId                  DeletarPostagemR GET DELETE

 /adm/inserir-postagem             NovaPostagemR    GET POST
 /adm/editar-postagem/#PostagemId  AlterarPostagemR GET POST

 /adm/gerenciar-contas                    AdminGerenciarContasR GET 
 /adm/gerenciar-contas/cadastrar-admin    AdminCadastrarR       POST
 /adm/gerenciar-contas/remover-admin      AdminRemoverR         POST

/static StaticR Static getStatic

-- /teste TestR GET
/formulario FormR GET POST
-- /authTest AuthTestR GET
|]

-- necessário para generalização de autenticidade do usuário para o Navbar
romefellerLayout :: Widget -> Handler Html
romefellerLayout widget = do
    pc             <- widgetToPageContent widget
    mauth          <- maybeAuth
    adminResult    <- ehAdmin
    mmsg           <- getMessage
    
    withUrlRenderer
        [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle pc}
                    <meta charset=utf-8>
                    ^{hHead}
                    ^{pageHead pc}
                <body>
                    <header>
                    $if adminResult == Authorized
                        ^{hAdminNav mauth}
                    $else
                        ^{hNav mauth}
                    $maybe msg <- mmsg
                        #{msg}
                    <article>
                        ^{pageBody pc}
                           
        |]

        
getAuthTestR :: Handler Html
getAuthTestR = undefined

instance RenderMessage RomefellerBlog FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist RomefellerBlog where
   type YesodPersistBackend RomefellerBlog = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

-- Montar p filtro para postagem de um comentário pelo usuário no arquivo postagem.hamlet
-- Caso o usuário não esteja logado não terá possibilidade de fazer um comentário.
instance Yesod RomefellerBlog where
    approot = ApprootStatic "https://rome-cannarozzo.c9users.io"
    defaultLayout = romefellerLayout
  --  yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
    authRoute _ = Just $ (AuthR LoginR)
    isAuthorized AdminR                _     = ehAdmin
    isAuthorized (DeletarPostagemR _ ) _     = ehAdmin
    isAuthorized NovaPostagemR         _     = ehAdmin
    isAuthorized (AlterarPostagemR _ ) _     = ehAdmin
    isAuthorized AdminGerenciarContasR _     = ehAdmin
    isAuthorized AdminCadastrarR       _     = ehAdmin
    isAuthorized AdminRemoverR         _     = ehAdmin    
-- Quaisquer rotas autorizadas para todos os usuários.
    isAuthorized _ _ = return Authorized

ehAdmin :: (Typeable (AuthEntity master),
           PersistEntity (AuthEntity master), YesodAuthPersist master,
           AuthId master ~ Key (AuthEntity master),
           AuthEntity master ~ Usuario,
           YesodPersistBackend master ~ SqlBackend) =>
        HandlerT master IO AuthResult
ehAdmin = do
    mauth <- maybeAuth
    case mauth of
        Nothing -> return AuthenticationRequired
        Just (Entity uid user)
            | (usuarioEmail user) /= (pack "") -> do
                    madmin <- runDB $ getBy $ UniqueAdministrador $ usuarioEmail user
                    case madmin of
                        Nothing            -> return $ Unauthorized (pack "Não tem permissão")
                        Just (Entity _ _ ) -> return Authorized
            | otherwise                        ->  
                return $ Unauthorized (pack "Você não possui permissão para acessar essa página")

  
-- == Auth AuthEmail 
instance YesodAuth RomefellerBlog where
    type AuthId RomefellerBlog = UsuarioId 
    loginDest _ = BlogR
    onLogin = do
        autorizacaoResult <- ehAdmin
        case autorizacaoResult of
            Unauthorized _         -> redirect BlogR
            Authorized             -> redirect AdminR
            AuthenticationRequired -> redirect (AuthR LoginR) 
                
    logoutDest  _ = BlogR
    authPlugins _ = [authGoogleEmailSaveToken clientId clientSecret, authEmail]
    authHttpManager = httpManager
    getAuthId creds = runDB $ do
        mtoken <- getUserAccessToken
        mname <- case mtoken of
            Nothing -> return Nothing
            Just token -> do
                master  <- lift getYesod
                mperson <- lift $ getPerson (authHttpManager master) token
                case mperson of
                    Nothing -> return Nothing
                    Just person -> return $ (personDisplayName) person 
            
        --liftIO $ print "!!!!!!!---------   getAuthId creds"
        resposta <- insertBy $ Usuario (credsIdent creds) Nothing  Nothing False mname
        return $ Just $
            case resposta of
                Left (Entity usuarioId _) -> usuarioId -- usuario adicionado recentemente
                Right usuarioId -> usuarioId -- usuário existente
             
    
instance YesodAuthPersist RomefellerBlog    

-- Quando um usuário solicita um novo cadastro, seu registro e efetuado pela API na tabela Usuario.
--  mesmo que o usuário não complete o cadastro o registro permanece. Veriricar uma forma para remover o registro caso o usuário não complete o cadastro após um determinado tempo.
instance YesodAuthEmail RomefellerBlog where
    type AuthEmailId RomefellerBlog = UsuarioId

    afterPasswordRoute _ = BlogR

    addUnverified email verkey =
        runDB $ insert $ Usuario email Nothing (Just verkey) False Nothing

    sendVerifyEmail email _ verurl = do
        -- Print out to the console the verification email, for easier
        -- debugging.
        liftIO $ putStrLn $ "Copy/ Paste this URL in your browser:" ++ unpack verurl

        -- Send email.
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                [stext|
                    Please confirm your email address by clicking on the link below.

                    #{verurl}

                    Thank you
                |]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml
                [shamlet|
                    <p>Please confirm your email address by clicking on the link below.
                    <p>
                        <a href=#{verurl}>#{verurl}
                    <p>Thank you
                |]
            , partHeaders = []
            }
    getVerifyKey = runDB . fmap (join . fmap usuarioToken) . get
    setVerifyKey uid key = runDB $ update uid [UsuarioToken =. Just key]
    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just u -> do
                update uid [UsuarioVerificado =. True]
                return $ Just uid
    getPassword = runDB . fmap (join . fmap usuarioSenha) . get
    setPassword uid pass = runDB $ update uid [UsuarioSenha =. Just pass]
    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueUsuario email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ usuarioSenha u
                , emailCredsVerkey = usuarioToken u
                , emailCredsEmail = email
                }
    getEmail = runDB . fmap (fmap usuarioEmail) . get
-- ====================================

    
    
{- ===========================================
    Forms
============================================== -}

-- ===================================================================Teste rota /formulario!!!!!

formPostagem
  :: (Monad m, RenderMessage (HandlerSite m) FormMessage) =>
     Key Usuario -> UTCTime -> FormInput m Postagem
formPostagem usuarioId tempoCorrente = Postagem
    <$> pure (usuarioId)
    <*> fmap (read.unpack) (ireq hiddenField "visivel") -- pode se utilizar boolField...
    <*> fmap (read.unpack) (ireq textField "categoria")
    <*> ireq textField "titulo"
    <*> pure tempoCorrente
    <*> ireq textareaField "conteudo"

formComentario
  :: (Monad m, RenderMessage (HandlerSite m) FormMessage) =>
     Key Postagem -> UTCTime -> Key Usuario -> FormInput m Comentario
formComentario postagemId tempoCorrente usuarioId = Comentario
    <$> pure (postagemId)
    <*> iopt hiddenField "postagemid"
    <*> pure tempoCorrente
    <*> pure usuarioId
    <*> ireq textareaField "conteudo"


getFormR :: Handler Html
getFormR = do
    entityComentarios <- runDB $ selectList [] [Desc ComentarioData]
    defaultLayout $ do 
        [whamlet| 
            $forall (Entity comentarioId comentario ) <- entityComentarios
                <p>#{show comentario}
        |]
        [whamlet| 
            <p>
                Dentro da tag p
            <form method=post action=@{FormR}>
                nome
                <input type=text name=nome>
                idade
                <input type=text name=idade>
                
                <input type=submit value=enviar>
        |]
    
postFormR :: Handler Html
postFormR = do
    (nome,idade) <-runInputPost $ (,) <$> ireq textField "nome" <*> ireq textField "idade"
    defaultLayout [whamlet| 
        #{show nome} #{show idade}
    |]
{-
    maybeUsuario <- runDB $ selectFirst [UsuarioId ==. (toSqlKey 1)] []
    case maybeUsuario of
        Nothing -> defaultLayout [whamlet|erro |]
        Just (Entity usuarioId usuario) -> do
            tempo <- liftIO getCurrentTime
            postagem <- runInputGet $ (formPostagem usuarioId tempo)
            --tempo <- getCurrentTime
            -- <*> areq passwordField   (bfs ("Senha" :: Text)) Nothing
            --  postagemUsuario <- runInputGet ( Postagem <$> ireq hiddenField "" <*> ireq textField "titulo" <*> ireq (tempo::UTCTime) "Time"  <*> ireq  Field "conteudo") -- $ formPostagem $ entityKey usuario
            defaultLayout $ [whamlet| #{show $ postagemData postagem} |]
-}

-- ===========================================================================Teste!!!!!!!!!
    
{- =============================================
  Gets & Posts
============================================= -}


-- / root
getBlogR :: Handler Html
getBlogR = do
    -- Exibir o nome do usuário ao invés do e-mail
    mauth <- maybeAuth
    
    postagens    <- runDB $ selectList [] [Desc PostagemData]
    qtdCurtidas  <- runDB $ mapM (\entP -> count [CurtidaPostagemId ==. entityKey entP] ) postagens
    let postagemCurtidasZip = reverse $ zipPostagemQTDCurtidas postagens qtdCurtidas 
    
    defaultLayout $ do
        setTitle "Blog - Romefeller"
        wAll
        $(whamletFile "templates/blog.hamlet")
        
-- Helper
zipPostagemQTDCurtidas :: [Entity Postagem] -> [Int] -> [(Entity Postagem, Int )]
zipPostagemQTDCurtidas (x:xs) (y:ys) = [(x , y )] ++ zipPostagemQTDCurtidas xs ys
zipPostagemQTDCurtidas  _ _          = []

getPostagemR :: PostagemId -> Handler Html
getPostagemR pid = do
    postagem <- runDB $ get404 pid
    let (ano, mes, dia) = toGregorian $ utctDay $ postagemData postagem 
    let mesLiteral = converterMesParaLiteral mes 

    comentariosUsuariosEntity <- runDB $ 
        (rawSql "SELECT ??, ?? FROM Comentario \
                             INNER JOIN Usuario \
                             ON Comentario.usuario_id=Usuario.id \
                             where Comentario.postagem_id = ? \
                             ORDER BY Comentario.data DESC" [toPersistValue pid])::Handler [(Entity Comentario, Entity Usuario)]
    
    
    liftIO $ print (show comentariosUsuariosEntity)
    mauthIdResult <- maybeAuthId
    let usuarioId = case mauthIdResult of
                Nothing  -> toSqlKey (-1) :: Key Usuario
                Just uid -> uid
    
    --Verifica se o usuário autenticado curtiu a postagem acessada. Caso sim o botão de curtir será desabilitado
    mcurtidaResult  <- runDB $ getBy $ ( UniqueCurtida pid usuarioId )
    {- Quantidade de Curtidas relacionada à postagem.
    foo <- runDB $ rawSql "select ?? FROM Curtida " [] :: Handler [(Entity Curtida)]
    liftIO $ print $ length $
                filter (\ b -> b == True ) $ 
                fmap (\ (Entity _ curtida ) -> curtidaPostagemId curtida == pid  ) foo
    -- qtdCurtidas <- runDB $ count ( [] :: [Filter Curtida] )
    -}
    qtdCurtidas <- runDB $ count [CurtidaPostagemId ==. pid ]
    
    defaultLayout $ do
        wAll
        wEstadoComentario
        wPostagemCurtir
        setTitle "Postagem - Romefeller"
        $(whamletFile "templates/postagem.hamlet")
            
postAdicionarComentarioR :: PostagemId -> Handler Html
postAdicionarComentarioR pid = do
    maybeUserId <- maybeAuthId
    case maybeUserId of
        Nothing -> 
            sendResponseStatus forbidden403 ("Ação proibida":: Text) -- 404. 
        Just uid -> do 
            agora <- liftIO getCurrentTime
            comentario <- runInputPost $ formComentario pid agora uid  
            runDB $ insert comentario
            setMessage "Comentario adicionado com sucesso"
            redirect $ PostagemR pid
            
-- Qual a necessidade do UsuarioId para remover comentário? Pode ser feito pegando o UsuarioId da Session    
getRemoverComentarioR :: ComentarioId -> UsuarioId -> PostagemId -> Handler ()
getRemoverComentarioR cid comentarioUserId pid = do
    mauthId <- maybeAuthId
    case mauthId of
        Nothing     -> sendResponseStatus status404 (pack "erro ao encontrar o ID do usuário autenticado")
        Just userId -> do
            case isUser of
                False -> 
                    sendResponseStatus status401 ("Se fudeu... precisa entrar com o usuário associado a postagem" :: Text)
                True  -> do
                    runDB $ delete cid 
                    setMessage "Postagem Deletada com Sucesso"
                    redirect ( PostagemR pid ) 
            where 
                isUser = userId == comentarioUserId                   
            
            
postEditarComentarioR :: ComentarioId -> UsuarioId -> PostagemId -> Handler ()
postEditarComentarioR comentarioId comentarioUsuarioId postagemId = do
    maybeUserId <- maybeAuthId
    case maybeUserId of
        Nothing ->
            sendResponseStatus forbidden403 ("Fudeu... aç��o proibida. Você precisa se autenticar" :: Text)
        Just userId -> do
            let isValidUser = userId == comentarioUsuarioId
            
            case isValidUser of
                False -> sendResponseStatus status401 ("Não é um usuário válido"::Text) -- Boilerplate
                True  -> do
                    agora <- liftIO getCurrentTime
                    comentarioAtualizado <- runInputPost $
                                                    formComentario (postagemId) agora comentarioUsuarioId  -- Só é utilizado o ComentárioConteúdo. Os outros campos são usados em outras partes do Handler. Pode ser substituído por uma tupla
                    runDB $ update comentarioId [ComentarioConteudo =. comentarioConteudo comentarioAtualizado]
                    setMessage "Comentário editado com sucesso!"
                    redirect $ PostagemR postagemId
   

patchPostagemCurtirR :: PostagemId -> Handler Value
patchPostagemCurtirR pid = do
    mauthResp <- maybeAuth
    case mauthResp of
        Nothing -> sendResponseStatus notFound404 (object [pack "resp" .= pack "Id do usuário não encontrado"])
        Just (Entity uid usuario ) -> do
            postagem <- runDB $ get404 pid
            runDB $ insert (Curtida pid uid)
            qtdCurtidas <- runDB $ count [CurtidaPostagemId ==. pid]
            sendResponseStatus ok200 (object [pack "resp" .= pack (show qtdCurtidas) ] ) -- retorno da quantidade de curtidas
            
          
-- /adm/                
-- Mostra as Postagens para os administradores. Tendo o poder de alterá-las e deletá-las
getAdminR :: Handler Html
getAdminR = do
    entityPostagens <- runDB $ selectList [] [Asc PostagemData]
    defaultLayout $ do
        wDeletarPostagem
        wAdminAll
        setTitle "Adm - Romefeller"
        $(whamletFile "templates/adm.hamlet")
        toWidget $ hAdminFloatingBtn

    
-- Verificar a Session é válida para efetuar a ação
-- Ou substituir <a href> por um button com form para fazer um post
getDeletarPostagemR :: PostagemId -> Handler Html
getDeletarPostagemR pid = do
    -- Administradores poderão deletar quaisquer postagem ou somente suas respectivas postagem?
    ---- Rotina que possibilide os administradores deleteram apenas suas respectivas postagens.
     runDB $ deleteCascade pid
     setMessage "Postagem deletada com Sucesso"
     redirect AdminR
     
deleteDeletarPostagemR :: PostagemId -> Handler Value
deleteDeletarPostagemR pid = do
    addHeader "Access-Control-Allow-Methods" "DELETE"
    runDB $ deleteCascade pid 
    checkPost <- runDB $ get pid
    case checkPost of
        Nothing -> sendResponseStatus ok200 (object [pack "resp" .= pack "Postagem removida"])  -- incluir o status correto
        Just _  -> sendResponseStatus status404 (object [pack "resp" .= pack "Erro: postagem não removida"])
    
getNovaPostagemR :: Handler Html
getNovaPostagemR = do
    defaultLayout $ do
        wAdminAll
        setTitle "Novo Post - Romefeller"
        $(whamletFile "templates/adm/nova-postagem.hamlet")
        toWidget $ hAdminFloatingBtn
    
postNovaPostagemR :: Handler ()
postNovaPostagemR = do
    maybeUserId <- maybeAuthId
    case maybeUserId of
        Nothing     -> 
            sendResponseStatus status404 (pack "Id do administrador não encontrado.")
        Just userId -> do
            agora <- liftIO getCurrentTime
            novaPostagem <- runInputPost $ formPostagem userId agora
            runDB $ insert novaPostagem
            setMessage "Postagem adicionada"
            redirect AdminR

        
getAlterarPostagemR :: PostagemId -> Handler Html
getAlterarPostagemR pid = do
    postagem <- runDB $ get404 pid
    defaultLayout $ do
        wAdminAll
        setTitle "Alterar Postagem -Romefeller"
        $(whamletFile "templates/adm/alterar-postagem.hamlet")
        toWidget $ hAdminFloatingBtn

postAlterarPostagemR :: PostagemId -> Handler ()
postAlterarPostagemR pid = do
    mauthIdResult <- maybeAuthId
    case mauthIdResult of
        Nothing -> 
            sendResponseStatus notFound404 (pack "Id do administrador não encontrado")
        Just usuarioId -> do
            agora <- liftIO getCurrentTime
            postagemAtualizada <- runInputPost $ formPostagem usuarioId agora --Key UsuarioId, virá da Session
            -- Adicionar um box/modal mostrando que a atualização foi bem sucedida.
            setMessage ("Postagem de título: " >>
                        (toHtml $ postagemTitulo postagemAtualizada) >>
                        " atualizado com sucesso!") 
            runDB $ replace pid $ postagemAtualizada 
            redirect AdminR

getAdminGerenciarContasR :: Handler Html
getAdminGerenciarContasR = do
    entityUsuarios <- runDB $ selectList [] [Asc UsuarioEmail]
    let emailUsuarios = map (usuarioEmail.entityVal) entityUsuarios
    mUserAdmin  <- runDB $ mapM (getBy.UniqueAdministrador) emailUsuarios
    
    let entityUsuariosMAdmin = zip entityUsuarios mUserAdmin
    --liftIO $ print (show usuarioEhAdmin)
    defaultLayout $ do 
        wAdminAll
        $(whamletFile "templates/adm/gerenciar-contas.hamlet") 
        toWidget $ hAdminFloatingBtn

postAdminCadastrarR :: Handler Html
postAdminCadastrarR = do
    admin <- runInputPost $ Administrador
                            <$> ireq textField "email"
                            
    muserResult <- runDB $ getBy $ UniqueUsuario (administradorEmail admin)
    case muserResult of
        Nothing            -> do
            setMessage "Erro! Não existe usuário cadastrado o email fornecido."
            redirect AdminGerenciarContasR
        Just (Entity _ _ ) -> do
            eAdminResult <- runDB $ insertBy admin
            case eAdminResult of
                Left (Entity aid admin ) -> do
                    setMessage "Erro! Este administrador já foi cadastrado."
                    redirect AdminGerenciarContasR
                Right aid                -> do
                    setMessage "Administrador Cadastrado com sucesso"
                    redirect AdminGerenciarContasR
                    
postAdminRemoverR :: Handler Html
postAdminRemoverR = do
    admin <- runInputPost $ Administrador 
                                     <$> ireq hiddenField "email"
    -- deleteBy $ UniqueAdministrador (administradorEmail admin)                                 
    mAdminResult <- runDB $ getBy $ UniqueAdministrador (administradorEmail admin) 
    case mAdminResult of
        Nothing -> do
            setMessage "Erro! Este usuário não esta credenciado como um Administrador"
            redirect AdminGerenciarContasR
        Just (Entity aid administrador) -> do
            runDB $ delete aid
            setMessage "Removida a credencial de Administrador do usuário com sucesso"
            redirect AdminGerenciarContasR

    
getTestR :: Handler Html
getTestR = do
   maid <- maybeAuthId 
   muser <- maybeAuth
   maybeToken <- getUserAccessToken
   defaultLayout [whamlet| 
       <p> MaybeauthId #{show maid}
       <p> Token #{show maybeToken}
       $maybe _ <- maid
           <p><a href=@{AuthR LogoutR}> Sair
       $nothing
           <p><a href=@{AuthR LoginR}>ir para a pagina de login
       <p>
       $maybe (Entity uid user) <-muser
           <p>#{show muser}
       $nothing 
           <p>sem usuário
        
   
   |]
    
    
{-============================================
    Helpers
============================================== -}

-- "AAAA-MM-DD" usada no postagem.hamlet
getdataFormatada :: IO String
getdataFormatada = do
    now <- getCurrentTime
    let today = utctDay now
    return $ showGregorian today

-- Usada no Handler getPostagemR
getAnoMesDiaDigito :: IO (Int,Int,Int)
getAnoMesDiaDigito = do
    agora <- getCurrentTime
    let hoje = utctDay agora
    let ( ano , mes, dia) = toGregorian hoje
    return $ (fromInteger ano, mes, dia)

-- Usada no Handler getPostagemR
-- fazer uma versao com tupla
converterMesParaLiteral :: Int -> MesLiteral
converterMesParaLiteral mes = do
    case mes of
        01 ->  Janeiro
        02 ->  Fevereiro
        03 ->  Março
        04 ->  Abril
        05 ->  Maio
        06 ->  Junho
        07 ->  Julho
        08 ->  Agosto
        09 ->  Setembro
        10 ->  Outubro
        11 ->  Novembro
        12 ->  Dezembro
        _  ->  ErroMes
    


{- =============================================
  Widgets && Hamlets
============================================= -}

hHead = do
    [hamlet|
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <meta name="theme-color" content="#fbc02d">
        
        <link rel="icon" href=@{StaticR img_favicon_ico} type="image/x-icon">
        <link rel="stylesheet" href=@{StaticR css_materialize_min_css}>
        <link rel="stylesheet" href=@{StaticR css_blog_css}>
        <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
    |]

wStyle :: Widget
wStyle = toWidget $(luciusFile "templates/blog.lucius")

hNav mauth = $(hamletFile "templates/navigation.hamlet")

wModalContact :: Widget
wModalContact = toWidget $(whamletFile "templates/modal-contact.hamlet")

wScript :: Widget
wScript = do
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"
    addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.6/js/materialize.min.js"
    toWidget [julius|
        $('.button-collapse').sideNav();
        $('.scrollspy').scrollSpy();
        
        $(document).ready(function(){
            $('.modal-trigger').leanModal();
        });
        
        $('.modal-trigger').leanModal({
            dismissible: true,
            opacity: .5,
            in_duration: 300,
            out_duration: 200,
            ready: function() { console.log('Modal aberto'); }, // Callback Modal aberto
            complete: function() { console.log('Modal fechado'); } // Callback Modal fechado
        });
    |]



wAll :: Widget
wAll = wModalContact >>
       wStyle >>
       wScript
       
-- Maybe (Entity Usuario) -> Widget’      
hAdminNav mauth = $(hamletFile "templates/navigation-adm.hamlet")

hAdminFloatingBtn = $(hamletFile "templates/adm-floating-btn.hamlet")

wModaisAll :: Widget
wModaisAll = toWidget $(whamletFile "templates/modais.hamlet")

wAdminAll :: Widget
wAdminAll = wStyle     >>
            wModaisAll >>
            wScript 

--usado por pathPostagemCurtir
wPostagemCurtir :: Widget
wPostagemCurtir = do
    toWidgetHead 
        [julius| 
            function curtirPostagem(postagemId){
                $.ajax({
                    url: 'https://rome-cannarozzo.c9users.io/postagem/curtir/'+postagemId,
                    type: 'patch',
                    success: function(data){
                        var qtdCurtidasServidor = data.resp;
                        $("#qtdCurtidas").html(qtdCurtidasServidor);
                        $("#btn-curtir").addClass('btn-floating disabled').attr('onclick', '');
                       
                        },
                        
                    error: function(xhr,status,error){
                                console.log('error',status);
                        },
                });
            }
        
        |]

--usado por deleteDeletarPostagem
wDeletarPostagem :: Widget
wDeletarPostagem = do
    toWidgetHead
            [julius|
                function pegarPostagemId(postagemId){
                                $.ajax({ 
                                   url: '/adm/'+postagemId,
                                   type: 'delete',
                                   processData: false,
                                   success: function (data) {
                                        console.log('success', data);
                                        alert(data.resp);
                                        $("tr").remove("#"+postagemId);
                                      },
                                   error: function (xhr,status,error){ 
                                      console.log('error',status);
                                      },             
                                 });
                    }
            |]

-- colocar dentro da rota onde ele é usado pois, será utilizado em um local.
wEstadoComentario :: Widget
wEstadoComentario = toWidget [julius|
    function mudaEstadoComentario(id){
        var output = this.document.getElementById("output"+id);
        var input = this.document.getElementById("input"+id);
        switch(output.style.display){
            case 'none': 
                  output.style.display = 'block';
                  input.style.display = 'none'
                  break;
            case 'block': 
                  output.style.display = 'none';
                  input.style.display = 'block';
                  break;
            default:
                alert("Erro");
        }
    }
    |] 
        

connStr = "dbname=dbs10j40oj2r50 host=ec2-54-225-95-99.compute-1.amazonaws.com user=ywzfzssfskwcju password=QOoBT4kER9KgAod70niCY3T6tB port=5432"

main :: IO ()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       flip runSqlPersistMPool pool $ do 
           runMigration migrateAll
           --insert $ Administrador (pack "felipe.cannarozzo@gmail.com")
       s <- static "static"
       manager <- newManager tlsManagerSettings
       warp 8080 (RomefellerBlog pool s manager)
