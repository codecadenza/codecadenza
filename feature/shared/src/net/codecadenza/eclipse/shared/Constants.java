/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.shared;

import java.nio.charset.StandardCharsets;

/**
 * <p>
 * Global constants that are used within all plug-ins!
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Constants {
	/**
	 * Prevent instantiation
	 */
	private Constants() {

	}

	public static final String DIAGRAM_FILE_EXTENSION = "ccd";
	public static final String MODEL_FILE_EXTENSION = "xmi";
	public static final String JAVA_RESOURCE_SUFFIX = ".java";
	public static final String MODEL_ROOT_FILE = "project." + MODEL_FILE_EXTENSION;
	public static final String POM_XML = "pom.xml";

	// Preference names for data source settings
	public static final String PREF_USER_NAME = "user";
	public static final String PREF_PASSWORD = "password";
	public static final String PREF_CONNECTION_URL = "connection_url";
	public static final String PREF_ENABLE_CACHE = "cache";
	public static final String PREF_DB_VENDOR = "vendor";

	// Preference names for contexts
	public static final String PREF_ROOT_CONTEXT = "root_context";
	public static final String PREF_REPOSITORY_CONTEXT = "repository_context";
	public static final String PREF_DTO_CONTEXT = "dto_context";
	public static final String PREF_EXC_CONTEXT = "exc_context";
	public static final String PREF_DOMAIN_CONTEXT = "domain_context";
	public static final String PREF_VALID_CONTEXT = "val_context";
	public static final String PREF_CONV_CONTEXT = "conv_context";
	public static final String PREF_BOUNDARY_CONTEXT = "bound_context";
	public static final String PREF_FACADE_CONTEXT = "facade_context";
	public static final String PREF_EXCHANGE_CONTEXT = "exchange_context";
	public static final String PREF_INTEGR_CONTEXT_REST = "integr_rest_context";
	public static final String PREF_INTEGR_CONTEXT_SOAP = "integr_soap_context";
	public static final String PREF_INTEGR_CONTEXT_RMI = "integr_rmi_context";
	public static final String PREF_INTEGR_CONTEXT_KAFKA = "integr_kafka_context";
	public static final String PREF_INTEGR_CONTEXT_JMS = "integr_jms_context";
	public static final String PREF_TEST_CONTEXT_SELENIUM = "test_selenium_context";

	// Preference names for database queries
	public static final String PREF_MAX_ROW_COUNT = "max_rows";
	public static final String PREF_DATE_FORMAT = "date_format";
	public static final String PREF_TIME_FORMAT = "time_format";
	public static final String PREF_NUMBER_FORMAT = "number_format";

	// Preference names for reverse engineering
	public static final String PREF_ATTR_NAME_TRACK_VERSION = "rev_eng_attr_name_track_version";
	public static final String PREF_ATTR_NAME_DATE_ON_PERSIST = "rev_eng_attr_name_date_on_persist";
	public static final String PREF_ATTR_NAME_DATE_ON_UPDATE = "rev_eng_attr_name_date_on_update";
	public static final String PREF_DEEP_SEARCH = "rev_eng_deep_search";
	public static final String PREF_START_WITH = "rev_eng_start_with";
	public static final String PREF_END_WITH = "rev_eng_end_with";
	public static final String PREF_NOT_START_WITH = "rev_eng_not_start_with";
	public static final String PREF_NOT_END_WITH = "rev_eng_not_end_with";

	// Preference names for test modules
	public static final String PREF_TEST_CASE_SUFFIX = "test_case_suffix";

	// Integration client base class names
	public static final String BASE_SOAP_CLIENT_CLASS_NAME = "AbstractSOAPClient";
	public static final String BASE_REST_CLIENT_CLASS_NAME = "AbstractRESTClient";
	public static final String BASE_RMI_CLIENT_CLASS_NAME = "AbstractRMIClient";
	public static final String BASE_KAFKA_CLIENT_CLASS_NAME = "AbstractKAFKAClient";
	public static final String BASE_JMS_CLIENT_CLASS_NAME = "AbstractJMSClient";

	// Standard exception names
	public static final String REMOTE_OPERATION_EXCEPTION_NAME = "RemoteOperationException";

	// Standard name for the REST application class
	public static final String REST_APPLICATION_CLASS_NAME = "RESTApplication";

	public static final String DEFAULT_XML_NS = "http://www.mydomain.abc/xml";
	public static final String DEFAULT_XML_PREFIX = "pre";

	// Name of parameter that holds search input data
	public static final String SEARCH_PARAM_NAME = "searchInput";
	public static final String INTEGRATION_SEARCH_PARAM_TYPE = "SearchInput";

	// Standard Java package names
	public static final String PACK_JAVA_LANG = "java.lang";
	public static final String PACK_JAVA_UTIL = "java.util";
	public static final String PACK_JAVA_MATH = "java.math";
	public static final String PACK_JAVA_TIME = "java.time";
	public static final String PACK_JAVA_TIME_FORMAT = PACK_JAVA_TIME + ".format";

	public static final String ICON_FOLDER = "icons";
	public static final String MODEL_FOLDER = "model";
	public static final String SQL_FOLDER = "sql";
	public static final String JPA_FOLDER = "jpa";
	public static final String LIB_FOLDER = "lib";
	public static final String SRC_FOLDER = "src";
	public static final String SCHEMA_FOLDER = "schema";
	public static final String META_INF_FOLDER = "META-INF";
	public static final String WEB_INF_FOLDER = "WEB-INF";
	public static final String OSGI_INF_FOLDER = "OSGI-INF";
	public static final String CONFIG_FOLDER = "config";
	public static final String TEST_DATA_FOLDER = "testdata";
	public static final String CLASSES_FOLDER = "classes";
	public static final String JAVA_FOLDER = "java";
	public static final String RESOURCES_FOLDER = "resources";
	public static final String WEB_APP_FOLDER = "webapp";
	public static final String SRC_MAIN_FOLDER = SRC_FOLDER + "/main";
	public static final String SRC_TEST_FOLDER = SRC_FOLDER + "/test";
	public static final String SRC_MAIN_JAVA_FOLDER = SRC_MAIN_FOLDER + "/" + JAVA_FOLDER;
	public static final String SRC_TEST_JAVA_FOLDER = SRC_TEST_FOLDER + "/" + JAVA_FOLDER;
	public static final String SRC_WEBAPP_FOLDER = SRC_MAIN_FOLDER + "/" + WEB_APP_FOLDER;
	public static final String SRC_MAIN_RESOURCES_FOLDER = SRC_MAIN_FOLDER + "/" + RESOURCES_FOLDER;
	public static final String SRC_TEST_RESOURCES_FOLDER = SRC_TEST_FOLDER + "/" + RESOURCES_FOLDER;
	public static final String RCP_ICON_FOLDER = ICON_FOLDER + "/full/obj16/";

	public static final String DEFAULT_DS = "defaultDS";

	// Suffix for libraries that are necessary for Eclipse RCP/RAP projects
	public static final String LIB_SUFFIX = "-1.0.0.jar";

	public static final String DEFAULT_IMAGE_CACHE_NAME = "ApplicationImageCache";
	public static final String DEFAULT_LOG_ON_DLG_NAME = "LogOnDialog";
	public static final String DEFAULT_APPLICATION_NAME = "Application";
	public static final String DEFAULT_PLUGIN_ID = "app.plugin.id";
	public static final String DEFAULT_ACTIVATOR_NAME = "Activator";
	public static final String DEFAULT_E4_LIFE_CYCLE_NAME = "E4LifeCycle";
	public static final String DEFAULT_PART_STACK_ID = "partstack.default";
	public static final String DEFAULT_RAP_APPLICATION_NAME = "BasicApplication";
	public static final String DEFAULT_RAP_CONTEXT_PATH = "/app";
	public static final String FORMAT_HANDLER_NAME = "FormatHandler";
	public static final String CHANGE_PASSWORD_HANDLER_NAME = "ChangePasswordHandler";
	public static final String APPLICATION_MODEL_NAME = "Application.e4xmi";
	public static final String JAVAFX_LAUNCHER_NAME = "AppLauncher";
	public static final String BUNDLE_NAME = "translation";
	public static final String TRANSLATION_FILE_NAME = BUNDLE_NAME + "_en.properties";
	public static final String EL_I18N_VAR = "i18n";

	public static final String DEFAULT_REPOSITORY = "repository";

	public static final String UI_APP_FOLDER = "/app";
	public static final String UI_LOV_FOLDER = "/lov";
	public static final String UI_DIALOG_FOLDER = "/dialog";
	public static final String UI_VIEW_FOLDER = "/view";
	public static final String UI_TREE_FOLDER = "/tree";
	public static final String UI_PANEL_FOLDER = UI_DIALOG_FOLDER + "/panel";

	public static final String ANGULAR_APP_FOLDER = "/src/app";
	public static final String ANGULAR_COMMON_FOLDER = ANGULAR_APP_FOLDER + "/common";
	public static final String ANGULAR_COMMON_MODEL_FOLDER = ANGULAR_COMMON_FOLDER + "/model";
	public static final String ANGULAR_COMMON_COMPONENTS_FOLDER = ANGULAR_COMMON_FOLDER + "/components";
	public static final String ANGULAR_COMMON_SERVICES_FOLDER = ANGULAR_COMMON_FOLDER + "/services";
	public static final String ANGULAR_DOMAIN_FOLDER = ANGULAR_APP_FOLDER + "/domain";
	public static final String ANGULAR_SERVICE_FOLDER = ANGULAR_APP_FOLDER + "/services";
	public static final String ANGULAR_PAGE_FOLDER = ANGULAR_APP_FOLDER + "/pages";
	public static final String ANGULAR_ENVIRONMENTS_FOLDER = "/src/environments";
	public static final String ANGULAR_LOCALE_FOLDER = "/src/locale";

	public static final String CONTRIBUTION_XML = "contribution.xml";
	public static final String PLUGIN_XML = "plugin.xml";

	public static final String LISTENER_SUFFIX = "CallbackListener";
	public static final String CONVERSION_SUFFIX = "Converter";

	public static final String BEAN_SUFFIX = "Bean";
	public static final String BOUNDARY_BEAN_SUFFIX = "BoundaryService" + BEAN_SUFFIX;
	public static final String BOUNDARY_SUFFIX = "BoundaryService";
	public static final String FACADE_BEAN_SUFFIX = "Facade" + BEAN_SUFFIX;
	public static final String FACADE_SUFFIX = "Facade";
	public static final String EXCHANGE_SUFFIX = "ExchangeService";
	public static final String LOGGING_BEAN_NAME = "LoggingService" + BEAN_SUFFIX;
	public static final String CHANGE_PWD_DLG_NAME = "ChangePasswordDialog";
	public static final String SAVED_QUERY_SERVICE = "SavedQueryService";
	public static final String SAVED_QUERY_SERVICE_BEAN = "SavedQueryService" + BEAN_SUFFIX;
	public static final String SECURITY_MANAGER = "SecurityManager";
	public static final String ECLIPSE_LINK_CONFIG = "EclipseLinkJPAConfiguration";
	public static final String MANAGED_SECURITY_MANAGER = "securityManager";
	public static final String PACKAGE_INFO_FILE = "package-info.java";
	public static final String DEFAULT_MAPPING_OBJ_NAME = "mappingObject";
	public static final String DEFAULT_ROOT_MAPPING_OBJ_NAME = "rootMappingObject";

	// Default names for standard page objects
	public static final String LOGIN_PAGE_NAME = "LoginPage";
	public static final String INDEX_PAGE_NAME = "IndexPage";

	// ID of the domain object diagram editor
	public static final String DIAGRAM_EDITOR_ID = "net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorID";

	// Image names
	public static final String IMG_COLUMN_ADD = "column_add.png";
	public static final String IMG_COLUMN_DELETE = "column_delete.png";
	public static final String IMG_COLUMN_EDIT = "column_edit.png";
	public static final String IMG_COLUMN = "column.png";
	public static final String IMG_KEY_ADD = "key_add.png";
	public static final String IMG_KEY_DELETE = "key_delete.png";
	public static final String IMG_KEY_EDIT = "key_edit.png";
	public static final String IMG_KEY = "key.png";
	public static final String IMG_INDEX_ADD = "index_add.png";
	public static final String IMG_INDEX_DELETE = "index_delete.png";
	public static final String IMG_INDEX_EDIT = "index_edit.png";
	public static final String IMG_INDEX = "index.png";
	public static final String IMG_PERFORM_SYNC = "exec.png";
	public static final String IMG_REFRESH = "refresh.gif";
	public static final String IMG_TABLE_DELETE = "table_delete.png";
	public static final String IMG_TABLE_NEW = "table_new.png";
	public static final String IMG_TABLE_REPLACE = "table_replace.png";
	public static final String IMG_TABLE = "table.png";
	public static final String IMG_EXPORT_CSV = "export_csv.png";
	public static final String IMG_CLIENT_CHECK = "checked.gif";
	public static final String IMG_CLIENT_UNCHECK = "unchecked.gif";
	public static final String IMG_CLIENT_TEXT = "text_field.gif";
	public static final String IMG_CLIENT_TREE = "tree.png";
	public static final String IMG_WINDOW_VIEW = "window_view.png";
	public static final String IMG_WINDOW_ADD = "window_add.png";
	public static final String IMG_WINDOW_EDIT = "window_edit.png";
	public static final String IMG_WINDOW_CREATE = "window_create.png";
	public static final String IMG_WINDOW_READ = "window_read.png";
	public static final String IMG_WINDOW_LOV = "window_lov.png";
	public static final String IMG_PANEL = "panel.png";
	public static final String IMG_TILE_BAR_BUTTONS = "title_bar_buttons.png";
	public static final String IMG_ARROW_LEFT = "arrow_left.png";
	public static final String IMG_ARROW_RIGHT = "arrow_right.png";
	public static final String IMG_UPLOAD = "upload.png";
	public static final String IMG_DOWNLOAD = "download.png";
	public static final String IMG_SEARCH = "search.png";

	// Images that are used in domain diagrams
	public static final String IMG_DOMAIN_OBJECT = "domain_object.gif";
	public static final String IMG_ABSTRACT_CLASS = "abstract_class.gif";
	public static final String IMG_ATTRIBUTE = "domain_attribute.gif";
	public static final String IMG_UK_ATTRIBUTE = "uk_attribute.gif";
	public static final String IMG_ID_ATTRIBUTE = "id_attribute.gif";
	public static final String IMG_INHERITANCE = "inheritance.gif";
	public static final String IMG_ENUM_ASSOC = "enum_assoc.gif";
	public static final String IMG_ENUM_LITERAL = "enum_literal.gif";
	public static final String IMG_ENUM = "enum.gif";
	public static final String IMG_DIAGRAM_SHORTCUT = "shortcut.gif";
	public static final String IMG_MTM_ASSOC = "mtm_assoc.gif";
	public static final String IMG_MTO_ASSOC = "mto_assoc.gif";
	public static final String IMG_OTM_ASSOC = "otm_assoc.gif";
	public static final String IMG_OTO_ASSOC = "oto_assoc.gif";

	// Images used by the SQL editor
	public static final String IMG_EXEC_SQL = "exec.png";

	// Special PostgreSQL column types
	public static final String POSTGRESQL_NUMERIC = "NUMERIC";
	public static final String POSTGRESQL_DECIMAL = "DECIMAL";

	// Special MS SQL column types
	public static final String MSSQL_BINARY = "BINARY";
	public static final String MSSQL_VARBINARY = "VARBINARY";

	// Standard suffix for database tables
	public static final String DB_TABLE_SUFFIX = "_tab";

	public static final String DB_FOREIGN_KEY_PREFIX = "fk_";
	public static final String DB_PRIMARY_KEY_PREFIX = "pk_";
	public static final String DB_UNIQUE_KEY_PREFIX = "uk_";
	public static final String DB_INDEX_PREFIX = "ind_";

	// Standard package fragments
	public static final String PACK_CLIENT_VIEW = ".view";
	public static final String PACK_CLIENT_PANEL = ".panel";
	public static final String PACK_CLIENT_DLG = ".dialog";
	public static final String PACK_CLIENT_LOV = ".lov";
	public static final String PACK_CLIENT_TREE = ".tree";
	public static final String PACK_CLIENT_CONVERTER = ".converter";
	public static final String PACK_PAGE_OBJECT = ".page";

	public static final String PACK_SERVICE = ".service";
	public static final String PACK_SERVICE_LOCATOR = "net.codecadenza.runtime.richclient.transport";
	public static final String PACK_SERVICE_LOCATOR_RAP = "net.codecadenza.runtime.richclient.eclipse.rap.services.ServiceLocator";
	public static final String PACK_VALIDATION = "net.codecadenza.runtime.validation";
	public static final String GENERATED_ELEMENT_ANNOTATION = "net.codecadenza.runtime.annotation.Generated";

	public static final String DEFAULT_SUB_PACKAGE = "base";
	public static final String SUB_PACKAGE_BEAN = ".bean";
	public static final String SUB_PACKAGE_LISTENER = ".listen";
	public static final String SUB_PACKAGE_INT_AVRO = ".avro";
	public static final String SUB_PACKAGE_INT_CLIENT = ".client";
	public static final String SUB_PACKAGE_INT_PRODUCER = ".producer";
	public static final String SUB_PACKAGE_INT_SECURITY = ".security";
	public static final String SUB_PACKAGE_INT_SEI = ".sei";
	public static final String SUB_PACKAGE_UTIL = ".util";

	// JDBC drivers
	public static final String DRIVER_ORACEL = "oracle.jdbc.OracleDriver";
	public static final String DRIVER_MYSQL = "com.mysql.cj.jdbc.Driver";
	public static final String DRIVER_DERBY = "org.apache.derby.jdbc.ClientDriver";
	public static final String DRIVER_DERBY_EMBEDDED = "org.apache.derby.jdbc.EmbeddedDriver";
	public static final String DRIVER_POSTGRESQL = "org.postgresql.Driver";
	public static final String DRIVER_MSSQL = "com.microsoft.sqlserver.jdbc.SQLServerDriver";

	public static final String CHECKBOX_PREFIX = "chk";
	public static final String EDITOR_PREFIX = "editor";
	public static final String LIST_PREFIX = "list";
	public static final String TEXT_PREFIX = "txt";
	public static final String COMBO_PREFIX = "cbo";
	public static final String GRID_DATA_PREFIX = "gd";
	public static final String BUTTON_PREFIX = "cmd";
	public static final String TABLE_PREFIX = "table";
	public static final String LOV_PREFIX = "lov";
	public static final String LIST_DTO_SUFFIX = "ListDTO";
	public static final String DTO_SUFFIX = "DTO";
	public static final String INITIAL_ONE_TO_MANY_DTO_SUFFIX = "InitDTO";
	public static final String CREATE_DTO_SUFFIX = "CreateDTO";
	public static final String UPDATE_DTO_SUFFIX = "UpdateDTO";
	public static final String EXISTING_OBJ_PREFIX = "existing";

	// Standard prefixes for boundary, facade and repository method names
	public static final String METHOD_PREFIX_COUNT = "count";
	public static final String METHOD_PREFIX_DELETE = "delete";
	public static final String METHOD_PREFIX_EXIST = "exists";
	public static final String METHOD_PREFIX_GET = "get";
	public static final String METHOD_PREFIX_FIND = "find";
	public static final String METHOD_PREFIX_SEARCH = "search";
	public static final String METHOD_PREFIX_QUERY = "query";
	public static final String METHOD_PREFIX_CHANGE = "set";
	public static final String METHOD_PREFIX_ADD = "add";
	public static final String METHOD_PREFIX_REMOVE = "remove";
	public static final String METHOD_PREFIX_SAVE = "save";

	// Default repository method names
	public static final String REPO_METHOD_NAME_CONTAIN = METHOD_PREFIX_EXIST + "ById";
	public static final String REPO_METHOD_NAME_COUNT = METHOD_PREFIX_COUNT;
	public static final String REPO_METHOD_NAME_COPY = "copy";
	public static final String REPO_METHOD_NAME_DELETE = METHOD_PREFIX_DELETE;
	public static final String REPO_METHOD_NAME_DELETE_ALL = METHOD_PREFIX_DELETE + "All";
	public static final String REPO_METHOD_NAME_DELETE_ENTITY = METHOD_PREFIX_DELETE + "Entity";
	public static final String REPO_METHOD_NAME_FIND_ALL = METHOD_PREFIX_FIND + "All";
	public static final String REPO_METHOD_NAME_FIND_BY_ID = METHOD_PREFIX_FIND + "ById";
	public static final String REPO_METHOD_NAME_GET_REFERENCE = "getReference";
	public static final String REPO_METHOD_NAME_PERSIST = "persist";
	public static final String REPO_METHOD_NAME_MERGE = "merge";
	public static final String REPO_METHOD_NAME_SEARCH = "search";
	public static final String REPO_METHOD_NAME_SAVE = "save";

	public static final String ACTION_PREFIX = "action";
	public static final String ACTION_CREATE = ACTION_PREFIX + "Create";
	public static final String ACTION_DELETE = ACTION_PREFIX + "Delete";
	public static final String ACTION_UPDATE = ACTION_PREFIX + "Update";
	public static final String ACTION_GET = ACTION_PREFIX + "Get";
	public static final String ACTION_ADD = ACTION_PREFIX + "Add";

	public static final String NAV_VIEW_NAME = "NavigatorView";
	public static final String NAV_PANEL_NAME = "NavigatorPanel";
	public static final String APP_LOGON_DTO_NAME = "ApplicationLogOnDTO";
	public static final String APP_LOGON_EXCEPTION_NAME = "SecurityException";

	public static final String APP_I18N_PROVIDER_CLASS = "I18NApp";
	public static final String FIELD_LABEL_VALIDATION = "fieldLabel";

	public static final String CONT_PATH_JRE = "org.eclipse.jdt.launching.JRE_CONTAINER";
	public static final String CONT_PATH_PDE = "org.eclipse.pde.core.requiredPlugins";
	public static final String CONT_PATH_MAVEN = "org.eclipse.m2e.MAVEN2_CLASSPATH_CONTAINER";
	public static final String CONT_PATH_JUNIT = "org.eclipse.jdt.junit.JUNIT_CONTAINER/5";
	public static final String PDE_NATURE_ID = "org.eclipse.pde.PluginNature";
	public static final String JAVA_NATURE_ID = "org.eclipse.jdt.core.javanature";
	public static final String MAVEN_NATURE_ID = "org.eclipse.m2e.core.maven2Nature";
	public static final String UTF_8 = StandardCharsets.UTF_8.name();

	// Supported data exchange modes
	public static final String EXCHANGE_MODE_FILE = "FILE";
	public static final String EXCHANGE_MODE_STRING = "STRING";
	public static final String EXCHANGE_MODE_DIRECT = "DIRECT";

	// Default parameter name that provides the fully qualified path name for the file to be imported
	public static final String EXCHANGE_PATH_PARAM = "pathToFile";

	// Parameter name for the logged on user
	public static final String PARAM_LOGGED_ON_USER = "loggedOnUserId";

	// Regular expressions for artifact names
	public static final String JAVA_ARTIFACT_REGEX = "[A-Za-z0-9_\\-.]+";
	public static final String ANGULAR_ARTIFACT_REGEX = "[A-Za-z][A-Za-z0-9]*(-{1}[A-Za-z]+[0-9]*)*";

	// Supported invocation modes
	public static final String INVOCATION_MODE_DEFAULT = "DEFAULT";
	public static final String INVOCATION_MODE_ASYNC = "ASYNCHRONOUS";
	public static final String INVOCATION_MODE_SCHEDULED = "SCHEDULED";

	public static final String DEFAULT_PARAM_IMP_STRING = "importContent";
	public static final String DEFAULT_VALUE_FLAG = "USE_DEFAULT";

	public static final short MIN_CLASS_NAME_LENGTH = 2;
	public static final short MIN_ATTRIBUTE_NAME_LENGTH = 2;

	public static final String QUOTE = "\"";

	// Default panel titles
	public static final String ADMIN_PANEL_TITLE = "Administration data";
	public static final String BASIC_PANEL_TITLE = "Basic data";

	// Default initialization values of data grid and proposal text field components
	public static final boolean REMOVE_DEFAULT_MENU_ITEMS = false;
	public static final int MIN_FILTER_LENGTH = 1;
}
