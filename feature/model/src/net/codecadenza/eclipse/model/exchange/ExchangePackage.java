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
package net.codecadenza.eclipse.model.exchange;

import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.mapping.MappingPackage;
import net.codecadenza.eclipse.model.service.ServicePackage;
import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;

/**
 * The <b>Package</b> for the model. It contains accessors for the meta objects to represent
 * <ul>
 * <li>each class,</li>
 * <li>each feature of each class,</li>
 * <li>each enum,</li>
 * <li>and each data type</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.exchange.ExchangeFactory
 * @model kind="package"
 * @generated
 */
public interface ExchangePackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "exchange";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/exchange.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.exchange";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	ExchangePackage eINSTANCE = net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeServiceBeanImpl <em>Data Exchange
	 * Service Bean</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeServiceBeanImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeServiceBean()
	 * @generated
	 */
	int DATA_EXCHANGE_SERVICE_BEAN = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_SERVICE_BEAN__NAME = ServicePackage.SERVICE_BEAN__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_SERVICE_BEAN__COMMENT = ServicePackage.SERVICE_BEAN__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_SERVICE_BEAN__MAPPABLE = ServicePackage.SERVICE_BEAN__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_SERVICE_BEAN__PRIMITIVE = ServicePackage.SERVICE_BEAN__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_SERVICE_BEAN__NAMESPACE = ServicePackage.SERVICE_BEAN__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_SERVICE_BEAN__DOMAIN_OBJECT = ServicePackage.SERVICE_BEAN__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Interface Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_SERVICE_BEAN__INTERFACE_NAME = ServicePackage.SERVICE_BEAN__INTERFACE_NAME;

	/**
	 * The feature ID for the '<em><b>Data Exchange Methods</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Data Exchange Service Bean</em>' class
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_SERVICE_BEAN_FEATURE_COUNT = ServicePackage.SERVICE_BEAN_FEATURE_COUNT + 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl <em>Data Exchange
	 * Method</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeMethod()
	 * @generated
	 */
	int DATA_EXCHANGE_METHOD = 1;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__NAME = ServicePackage.SERVICE_METHOD__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__COMMENT = ServicePackage.SERVICE_METHOD__COMMENT;

	/**
	 * The feature ID for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__JAVA_TYPE = ServicePackage.SERVICE_METHOD__JAVA_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__RETURN_TYPE = ServicePackage.SERVICE_METHOD__RETURN_TYPE;

	/**
	 * The feature ID for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__RETURN_TYPE_MODIFIER = ServicePackage.SERVICE_METHOD__RETURN_TYPE_MODIFIER;

	/**
	 * The feature ID for the '<em><b>Method Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__METHOD_PARAMETERS = ServicePackage.SERVICE_METHOD__METHOD_PARAMETERS;

	/**
	 * The feature ID for the '<em><b>Query Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__QUERY_STATEMENT = ServicePackage.SERVICE_METHOD__QUERY_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Permission Mode</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__PERMISSION_MODE = ServicePackage.SERVICE_METHOD__PERMISSION_MODE;

	/**
	 * The feature ID for the '<em><b>Transaction Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__TRANSACTION_TYPE = ServicePackage.SERVICE_METHOD__TRANSACTION_TYPE;

	/**
	 * The feature ID for the '<em><b>Roles</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__ROLES = ServicePackage.SERVICE_METHOD__ROLES;

	/**
	 * The feature ID for the '<em><b>Method Invocation</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__METHOD_INVOCATION = ServicePackage.SERVICE_METHOD__METHOD_INVOCATION;

	/**
	 * The feature ID for the '<em><b>Custom Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__CUSTOM_STATEMENT = ServicePackage.SERVICE_METHOD__CUSTOM_STATEMENT;

	/**
	 * The feature ID for the '<em><b>Exchange Mode</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__EXCHANGE_MODE = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Root Element</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__ROOT_ELEMENT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Content Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__CONTENT_TYPE = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Method Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__METHOD_TYPE = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Parser</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__PARSER = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 4;

	/**
	 * The feature ID for the '<em><b>Perform Validation</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__PERFORM_VALIDATION = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 5;

	/**
	 * The feature ID for the '<em><b>Charset</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__CHARSET = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 6;

	/**
	 * The feature ID for the '<em><b>Format Output</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__FORMAT_OUTPUT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 7;

	/**
	 * The feature ID for the '<em><b>Data Exchange Service Bean</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 8;

	/**
	 * The feature ID for the '<em><b>Process Single Object</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__PROCESS_SINGLE_OBJECT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 9;

	/**
	 * The feature ID for the '<em><b>Joined Import Method</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__JOINED_IMPORT_METHOD = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 10;

	/**
	 * The feature ID for the '<em><b>Quote Character</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__QUOTE_CHARACTER = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 11;

	/**
	 * The feature ID for the '<em><b>Record Separator</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__RECORD_SEPARATOR = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 12;

	/**
	 * The feature ID for the '<em><b>Comment Character</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__COMMENT_CHARACTER = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 13;

	/**
	 * The feature ID for the '<em><b>Delimiter</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__DELIMITER = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 14;

	/**
	 * The feature ID for the '<em><b>Default Date Format</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__DEFAULT_DATE_FORMAT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 15;

	/**
	 * The feature ID for the '<em><b>Default Date Time Format</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__DEFAULT_DATE_TIME_FORMAT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 16;

	/**
	 * The feature ID for the '<em><b>Default Number Format</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__DEFAULT_NUMBER_FORMAT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 17;

	/**
	 * The feature ID for the '<em><b>Association Controllers</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__ASSOCIATION_CONTROLLERS = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 18;

	/**
	 * The feature ID for the '<em><b>Schema File Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD__SCHEMA_FILE_NAME = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 19;

	/**
	 * The number of structural features of the '<em>Data Exchange Method</em>' class
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_METHOD_FEATURE_COUNT = ServicePackage.SERVICE_METHOD_FEATURE_COUNT + 20;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl <em>Data Exchange
	 * Mode</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeMode()
	 * @generated
	 */
	int DATA_EXCHANGE_MODE = 2;

	/**
	 * The feature ID for the '<em><b>Max Objects To Be Processed</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED = 0;

	/**
	 * The number of structural features of the '<em>Data Exchange Mode</em>' class
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_MODE_FEATURE_COUNT = 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.StringExchangeModeImpl <em>String Exchange
	 * Mode</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.StringExchangeModeImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getStringExchangeMode()
	 * @generated
	 */
	int STRING_EXCHANGE_MODE = 3;

	/**
	 * The feature ID for the '<em><b>Max Objects To Be Processed</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int STRING_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED = DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED;

	/**
	 * The number of structural features of the '<em>String Exchange Mode</em>' class
	 * @generated
	 * @ordered
	 */
	int STRING_EXCHANGE_MODE_FEATURE_COUNT = DATA_EXCHANGE_MODE_FEATURE_COUNT + 0;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.FileExchangeModeImpl <em>File Exchange
	 * Mode</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.FileExchangeModeImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getFileExchangeMode()
	 * @generated
	 */
	int FILE_EXCHANGE_MODE = 4;

	/**
	 * The feature ID for the '<em><b>Max Objects To Be Processed</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILE_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED = DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED;

	/**
	 * The feature ID for the '<em><b>Path</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILE_EXCHANGE_MODE__PATH = DATA_EXCHANGE_MODE_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>File Name Pattern</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILE_EXCHANGE_MODE__FILE_NAME_PATTERN = DATA_EXCHANGE_MODE_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Block Size</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILE_EXCHANGE_MODE__BLOCK_SIZE = DATA_EXCHANGE_MODE_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>New Transaction Per File</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILE_EXCHANGE_MODE__NEW_TRANSACTION_PER_FILE = DATA_EXCHANGE_MODE_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Delete After Import</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILE_EXCHANGE_MODE__DELETE_AFTER_IMPORT = DATA_EXCHANGE_MODE_FEATURE_COUNT + 4;

	/**
	 * The feature ID for the '<em><b>Target Path After Import</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILE_EXCHANGE_MODE__TARGET_PATH_AFTER_IMPORT = DATA_EXCHANGE_MODE_FEATURE_COUNT + 5;

	/**
	 * The number of structural features of the '<em>File Exchange Mode</em>' class
	 * @generated
	 * @ordered
	 */
	int FILE_EXCHANGE_MODE_FEATURE_COUNT = DATA_EXCHANGE_MODE_FEATURE_COUNT + 6;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl <em>Data Exchange
	 * Element</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeElement()
	 * @generated
	 */
	int DATA_EXCHANGE_ELEMENT = 5;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Min Occurrences</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__MIN_OCCURRENCES = 1;

	/**
	 * The feature ID for the '<em><b>Max Occurrences</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__MAX_OCCURRENCES = 2;

	/**
	 * The feature ID for the '<em><b>Element Order</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__ELEMENT_ORDER = 3;

	/**
	 * The feature ID for the '<em><b>Type Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__TYPE_NAME = 4;

	/**
	 * The feature ID for the '<em><b>Wrapper Element Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__WRAPPER_ELEMENT_NAME = 5;

	/**
	 * The feature ID for the '<em><b>Data Exchange Method</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD = 6;

	/**
	 * The feature ID for the '<em><b>Value List Entries</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__VALUE_LIST_ENTRIES = 7;

	/**
	 * The feature ID for the '<em><b>Attributes</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__ATTRIBUTES = 8;

	/**
	 * The feature ID for the '<em><b>Mapping Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__MAPPING_ATTRIBUTE = 9;

	/**
	 * The feature ID for the '<em><b>Sub Elements</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS = 10;

	/**
	 * The feature ID for the '<em><b>Parent Element</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT = 11;

	/**
	 * The feature ID for the '<em><b>Data Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__DATA_TYPE = 12;

	/**
	 * The feature ID for the '<em><b>Mapping Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__MAPPING_OBJECT = 13;

	/**
	 * The feature ID for the '<em><b>Container</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__CONTAINER = 14;

	/**
	 * The feature ID for the '<em><b>Disable External Mapping</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__DISABLE_EXTERNAL_MAPPING = 15;

	/**
	 * The feature ID for the '<em><b>Used For Custom Query</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT__USED_FOR_CUSTOM_QUERY = 16;

	/**
	 * The number of structural features of the '<em>Data Exchange Element</em>' class
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ELEMENT_FEATURE_COUNT = 17;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl <em>Data Exchange
	 * Attribute</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeAttribute()
	 * @generated
	 */
	int DATA_EXCHANGE_ATTRIBUTE = 6;

	/**
	 * The feature ID for the '<em><b>Value List Entries</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__VALUE_LIST_ENTRIES = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__NAME = 1;

	/**
	 * The feature ID for the '<em><b>Visible</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__VISIBLE = 2;

	/**
	 * The feature ID for the '<em><b>Optional</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__OPTIONAL = 3;

	/**
	 * The feature ID for the '<em><b>Attribute Order</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__ATTRIBUTE_ORDER = 4;

	/**
	 * The feature ID for the '<em><b>Readonly</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__READONLY = 5;

	/**
	 * The feature ID for the '<em><b>Element</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__ELEMENT = 6;

	/**
	 * The feature ID for the '<em><b>Mapping Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__MAPPING_ATTRIBUTE = 7;

	/**
	 * The feature ID for the '<em><b>Data Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__DATA_TYPE = 8;

	/**
	 * The feature ID for the '<em><b>Format</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__FORMAT = 9;

	/**
	 * The feature ID for the '<em><b>Disable External Mapping</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__DISABLE_EXTERNAL_MAPPING = 10;

	/**
	 * The feature ID for the '<em><b>Used For Custom Query</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE__USED_FOR_CUSTOM_QUERY = 11;

	/**
	 * The number of structural features of the '<em>Data Exchange Attribute</em>' class
	 * @generated
	 * @ordered
	 */
	int DATA_EXCHANGE_ATTRIBUTE_FEATURE_COUNT = 12;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.ValueListEntryImpl <em>Value List Entry</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.exchange.impl.ValueListEntryImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getValueListEntry()
	 * @generated
	 */
	int VALUE_LIST_ENTRY = 7;

	/**
	 * The feature ID for the '<em><b>Item Text</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int VALUE_LIST_ENTRY__ITEM_TEXT = 0;

	/**
	 * The number of structural features of the '<em>Value List Entry</em>' class
	 * @generated
	 * @ordered
	 */
	int VALUE_LIST_ENTRY_FEATURE_COUNT = 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingObjectImpl <em>Mapping
	 * Object</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingObjectImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getExchangeMappingObject()
	 * @generated
	 */
	int EXCHANGE_MAPPING_OBJECT = 8;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT__NAME = MappingPackage.MAPPING_OBJECT__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT__COMMENT = MappingPackage.MAPPING_OBJECT__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT__MAPPABLE = MappingPackage.MAPPING_OBJECT__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT__PRIMITIVE = MappingPackage.MAPPING_OBJECT__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT__NAMESPACE = MappingPackage.MAPPING_OBJECT__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT__DOMAIN_OBJECT = MappingPackage.MAPPING_OBJECT__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Attributes</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT__ATTRIBUTES = MappingPackage.MAPPING_OBJECT_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Delete All Items</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT__DELETE_ALL_ITEMS = MappingPackage.MAPPING_OBJECT_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Add New Items</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT__ADD_NEW_ITEMS = MappingPackage.MAPPING_OBJECT_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Update Existing Items</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT__UPDATE_EXISTING_ITEMS = MappingPackage.MAPPING_OBJECT_FEATURE_COUNT + 3;

	/**
	 * The number of structural features of the '<em>Mapping Object</em>' class
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_OBJECT_FEATURE_COUNT = MappingPackage.MAPPING_OBJECT_FEATURE_COUNT + 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl <em>Mapping
	 * Attribute</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getExchangeMappingAttribute()
	 * @generated
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE = 9;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__NAME = MappingPackage.MAPPING_ATTRIBUTE__NAME;

	/**
	 * The feature ID for the '<em><b>Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__ASSOCIATION = MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION;

	/**
	 * The feature ID for the '<em><b>Domain Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE = MappingPackage.MAPPING_ATTRIBUTE__DOMAIN_ATTRIBUTE;

	/**
	 * The feature ID for the '<em><b>Association List</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__ASSOCIATION_LIST = MappingPackage.MAPPING_ATTRIBUTE__ASSOCIATION_LIST;

	/**
	 * The feature ID for the '<em><b>Mapping Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__MAPPING_TYPE = MappingPackage.MAPPING_ATTRIBUTE__MAPPING_TYPE;

	/**
	 * The feature ID for the '<em><b>Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__MODIFIER = MappingPackage.MAPPING_ATTRIBUTE__MODIFIER;

	/**
	 * The feature ID for the '<em><b>Default Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__DEFAULT_VALUE = MappingPackage.MAPPING_ATTRIBUTE__DEFAULT_VALUE;

	/**
	 * The feature ID for the '<em><b>Exchange Mapping Object</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Insertable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__INSERTABLE = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Updatable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__UPDATABLE = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Selection List Statement</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__SELECTION_LIST_STATEMENT = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Delete All Items</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__DELETE_ALL_ITEMS = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 4;

	/**
	 * The feature ID for the '<em><b>Add New Items</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__ADD_NEW_ITEMS = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 5;

	/**
	 * The feature ID for the '<em><b>Join Attribute</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__JOIN_ATTRIBUTE = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 6;

	/**
	 * The feature ID for the '<em><b>Update Existing Items</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE__UPDATE_EXISTING_ITEMS = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 7;

	/**
	 * The number of structural features of the '<em>Mapping Attribute</em>' class
	 * @generated
	 * @ordered
	 */
	int EXCHANGE_MAPPING_ATTRIBUTE_FEATURE_COUNT = MappingPackage.MAPPING_ATTRIBUTE_FEATURE_COUNT + 8;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.FilterMethodParameterImpl <em>Filter Method
	 * Parameter</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.FilterMethodParameterImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getFilterMethodParameter()
	 * @generated
	 */
	int FILTER_METHOD_PARAMETER = 10;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILTER_METHOD_PARAMETER__NAME = JavaPackage.METHOD_PARAMETER__NAME;

	/**
	 * The feature ID for the '<em><b>Method</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int FILTER_METHOD_PARAMETER__METHOD = JavaPackage.METHOD_PARAMETER__METHOD;

	/**
	 * The feature ID for the '<em><b>Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FILTER_METHOD_PARAMETER__TYPE = JavaPackage.METHOD_PARAMETER__TYPE;

	/**
	 * The feature ID for the '<em><b>Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILTER_METHOD_PARAMETER__MODIFIER = JavaPackage.METHOD_PARAMETER__MODIFIER;

	/**
	 * The feature ID for the '<em><b>Hint</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILTER_METHOD_PARAMETER__HINT = JavaPackage.METHOD_PARAMETER__HINT;

	/**
	 * The feature ID for the '<em><b>Domain Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FILTER_METHOD_PARAMETER__DOMAIN_ATTRIBUTE = JavaPackage.METHOD_PARAMETER_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int FILTER_METHOD_PARAMETER__ASSOCIATION = JavaPackage.METHOD_PARAMETER_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Association List</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int FILTER_METHOD_PARAMETER__ASSOCIATION_LIST = JavaPackage.METHOD_PARAMETER_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Operator</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int FILTER_METHOD_PARAMETER__OPERATOR = JavaPackage.METHOD_PARAMETER_FEATURE_COUNT + 3;

	/**
	 * The number of structural features of the '<em>Filter Method Parameter</em>' class
	 * @generated
	 * @ordered
	 */
	int FILTER_METHOD_PARAMETER_FEATURE_COUNT = JavaPackage.METHOD_PARAMETER_FEATURE_COUNT + 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.AssociationControllerImpl <em>Association
	 * Controller</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.AssociationControllerImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getAssociationController()
	 * @generated
	 */
	int ASSOCIATION_CONTROLLER = 11;

	/**
	 * The feature ID for the '<em><b>Query Attributes</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int ASSOCIATION_CONTROLLER__QUERY_ATTRIBUTES = 0;

	/**
	 * The feature ID for the '<em><b>Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ASSOCIATION_CONTROLLER__ASSOCIATION = 1;

	/**
	 * The feature ID for the '<em><b>Persist Attributes</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int ASSOCIATION_CONTROLLER__PERSIST_ATTRIBUTES = 2;

	/**
	 * The number of structural features of the '<em>Association Controller</em>' class
	 * @generated
	 * @ordered
	 */
	int ASSOCIATION_CONTROLLER_FEATURE_COUNT = 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.impl.DirectExchangeModeImpl <em>Direct Exchange
	 * Mode</em>}' class
	 * @see net.codecadenza.eclipse.model.exchange.impl.DirectExchangeModeImpl
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDirectExchangeMode()
	 * @generated
	 */
	int DIRECT_EXCHANGE_MODE = 12;

	/**
	 * The feature ID for the '<em><b>Max Objects To Be Processed</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DIRECT_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED = DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED;

	/**
	 * The number of structural features of the '<em>Direct Exchange Mode</em>' class
	 * @generated
	 * @ordered
	 */
	int DIRECT_EXCHANGE_MODE_FEATURE_COUNT = DATA_EXCHANGE_MODE_FEATURE_COUNT + 0;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration <em>Data Exchange
	 * Method Type Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeMethodTypeEnumeration()
	 * @generated
	 */
	int DATA_EXCHANGE_METHOD_TYPE_ENUMERATION = 13;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration <em>Content Type
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getContentTypeEnumeration()
	 * @generated
	 */
	int CONTENT_TYPE_ENUMERATION = 14;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration <em>Parser
	 * Implementation Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration
	 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getParserImplementationEnumeration()
	 * @generated
	 */
	int PARSER_IMPLEMENTATION_ENUMERATION = 15;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean <em>Data Exchange
	 * Service Bean</em>}'
	 * @return the meta object for class '<em>Data Exchange Service Bean</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean
	 * @generated
	 */
	EClass getDataExchangeServiceBean();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean#getDataExchangeMethods <em>Data Exchange Methods</em>}'
	 * @return the meta object for the containment reference list '<em>Data Exchange Methods</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean#getDataExchangeMethods()
	 * @see #getDataExchangeServiceBean()
	 * @generated
	 */
	EReference getDataExchangeServiceBean_DataExchangeMethods();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod <em>Data Exchange
	 * Method</em>}'
	 * @return the meta object for class '<em>Data Exchange Method</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod
	 * @generated
	 */
	EClass getDataExchangeMethod();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDataExchangeServiceBean <em>Data Exchange Service
	 * Bean</em>}'
	 * @return the meta object for the container reference '<em>Data Exchange Service Bean</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDataExchangeServiceBean()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EReference getDataExchangeMethod_DataExchangeServiceBean();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isProcessSingleObject <em>Process Single Object</em>}'
	 * @return the meta object for the attribute '<em>Process Single Object</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isProcessSingleObject()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_ProcessSingleObject();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getJoinedImportMethod <em>Joined Import Method</em>}'
	 * @return the meta object for the reference '<em>Joined Import Method</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getJoinedImportMethod()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EReference getDataExchangeMethod_JoinedImportMethod();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getQuoteCharacter
	 * <em>Quote Character</em>}'
	 * @return the meta object for the attribute '<em>Quote Character</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getQuoteCharacter()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_QuoteCharacter();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRecordSeparator
	 * <em>Record Separator</em>}'
	 * @return the meta object for the attribute '<em>Record Separator</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRecordSeparator()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_RecordSeparator();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getCommentCharacter
	 * <em>Comment Character</em>}'
	 * @return the meta object for the attribute '<em>Comment Character</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getCommentCharacter()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_CommentCharacter();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDelimiter
	 * <em>Delimiter</em>}'
	 * @return the meta object for the attribute '<em>Delimiter</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDelimiter()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_Delimiter();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultDateFormat <em>Default Date Format</em>}'
	 * @return the meta object for the attribute '<em>Default Date Format</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultDateFormat()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_DefaultDateFormat();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultDateTimeFormat <em>Default Date Time Format</em>}'
	 * @return the meta object for the attribute '<em>Default Date Time Format</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultDateTimeFormat()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_DefaultDateTimeFormat();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultNumberFormat <em>Default Number Format</em>}'
	 * @return the meta object for the attribute '<em>Default Number Format</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getDefaultNumberFormat()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_DefaultNumberFormat();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getAssociationControllers <em>Association Controllers</em>}'
	 * @return the meta object for the containment reference list '<em>Association Controllers</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getAssociationControllers()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EReference getDataExchangeMethod_AssociationControllers();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getSchemaFileName
	 * <em>Schema File Name</em>}'
	 * @return the meta object for the attribute '<em>Schema File Name</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getSchemaFileName()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_SchemaFileName();

	/**
	 * Return the meta object for the containment reference
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getExchangeMode <em>Exchange Mode</em>}'
	 * @return the meta object for the containment reference '<em>Exchange Mode</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getExchangeMode()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EReference getDataExchangeMethod_ExchangeMode();

	/**
	 * Return the meta object for the containment reference
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRootElement <em>Root Element</em>}'
	 * @return the meta object for the containment reference '<em>Root Element</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRootElement()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EReference getDataExchangeMethod_RootElement();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getContentType
	 * <em>Content Type</em>}'
	 * @return the meta object for the attribute '<em>Content Type</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getContentType()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_ContentType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getMethodType
	 * <em>Method Type</em>}'
	 * @return the meta object for the attribute '<em>Method Type</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getMethodType()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_MethodType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getParser
	 * <em>Parser</em>}'
	 * @return the meta object for the attribute '<em>Parser</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getParser()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_Parser();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isPerformValidation
	 * <em>Perform Validation</em>}'
	 * @return the meta object for the attribute '<em>Perform Validation</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isPerformValidation()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_PerformValidation();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getCharset
	 * <em>Charset</em>}'
	 * @return the meta object for the attribute '<em>Charset</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getCharset()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_Charset();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isFormatOutput
	 * <em>Format Output</em>}'
	 * @return the meta object for the attribute '<em>Format Output</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#isFormatOutput()
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	EAttribute getDataExchangeMethod_FormatOutput();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMode <em>Data Exchange Mode</em>}'
	 * @return the meta object for class '<em>Data Exchange Mode</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMode
	 * @generated
	 */
	EClass getDataExchangeMode();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMode#getMaxObjectsToBeProcessed <em>Max Objects To Be
	 * Processed</em>}'
	 * @return the meta object for the attribute '<em>Max Objects To Be Processed</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMode#getMaxObjectsToBeProcessed()
	 * @see #getDataExchangeMode()
	 * @generated
	 */
	EAttribute getDataExchangeMode_MaxObjectsToBeProcessed();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.StringExchangeMode <em>String Exchange
	 * Mode</em>}'
	 * @return the meta object for class '<em>String Exchange Mode</em>'
	 * @see net.codecadenza.eclipse.model.exchange.StringExchangeMode
	 * @generated
	 */
	EClass getStringExchangeMode();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode <em>File Exchange Mode</em>}'
	 * @return the meta object for class '<em>File Exchange Mode</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode
	 * @generated
	 */
	EClass getFileExchangeMode();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getPath
	 * <em>Path</em>}'
	 * @return the meta object for the attribute '<em>Path</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#getPath()
	 * @see #getFileExchangeMode()
	 * @generated
	 */
	EAttribute getFileExchangeMode_Path();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getFileNamePattern
	 * <em>File Name Pattern</em>}'
	 * @return the meta object for the attribute '<em>File Name Pattern</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#getFileNamePattern()
	 * @see #getFileExchangeMode()
	 * @generated
	 */
	EAttribute getFileExchangeMode_FileNamePattern();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getBlockSize
	 * <em>Block Size</em>}'
	 * @return the meta object for the attribute '<em>Block Size</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#getBlockSize()
	 * @see #getFileExchangeMode()
	 * @generated
	 */
	EAttribute getFileExchangeMode_BlockSize();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#isNewTransactionPerFile <em>New Transaction Per File</em>}'
	 * @return the meta object for the attribute '<em>New Transaction Per File</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#isNewTransactionPerFile()
	 * @see #getFileExchangeMode()
	 * @generated
	 */
	EAttribute getFileExchangeMode_NewTransactionPerFile();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#isDeleteAfterImport
	 * <em>Delete After Import</em>}'
	 * @return the meta object for the attribute '<em>Delete After Import</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#isDeleteAfterImport()
	 * @see #getFileExchangeMode()
	 * @generated
	 */
	EAttribute getFileExchangeMode_DeleteAfterImport();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.FileExchangeMode#getTargetPathAfterImport <em>Target Path After Import</em>}'
	 * @return the meta object for the attribute '<em>Target Path After Import</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FileExchangeMode#getTargetPathAfterImport()
	 * @see #getFileExchangeMode()
	 * @generated
	 */
	EAttribute getFileExchangeMode_TargetPathAfterImport();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement <em>Data Exchange
	 * Element</em>}'
	 * @return the meta object for class '<em>Data Exchange Element</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement
	 * @generated
	 */
	EClass getDataExchangeElement();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getName
	 * <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getName()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EAttribute getDataExchangeElement_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMinOccurrences
	 * <em>Min Occurrences</em>}'
	 * @return the meta object for the attribute '<em>Min Occurrences</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMinOccurrences()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EAttribute getDataExchangeElement_MinOccurrences();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMaxOccurrences
	 * <em>Max Occurrences</em>}'
	 * @return the meta object for the attribute '<em>Max Occurrences</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMaxOccurrences()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EAttribute getDataExchangeElement_MaxOccurrences();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getElementOrder
	 * <em>Element Order</em>}'
	 * @return the meta object for the attribute '<em>Element Order</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getElementOrder()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EAttribute getDataExchangeElement_ElementOrder();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getTypeName
	 * <em>Type Name</em>}'
	 * @return the meta object for the attribute '<em>Type Name</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getTypeName()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EAttribute getDataExchangeElement_TypeName();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getWrapperElementName <em>Wrapper Element Name</em>}'
	 * @return the meta object for the attribute '<em>Wrapper Element Name</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getWrapperElementName()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EAttribute getDataExchangeElement_WrapperElementName();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataExchangeMethod <em>Data Exchange Method</em>}'
	 * @return the meta object for the container reference '<em>Data Exchange Method</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataExchangeMethod()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EReference getDataExchangeElement_DataExchangeMethod();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getValueListEntries <em>Value List Entries</em>}'
	 * @return the meta object for the containment reference list '<em>Value List Entries</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getValueListEntries()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EReference getDataExchangeElement_ValueListEntries();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getAttributes <em>Attributes</em>}'
	 * @return the meta object for the containment reference list '<em>Attributes</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getAttributes()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EReference getDataExchangeElement_Attributes();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMappingAttribute <em>Mapping Attribute</em>}'
	 * @return the meta object for the reference '<em>Mapping Attribute</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMappingAttribute()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EReference getDataExchangeElement_MappingAttribute();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getSubElements <em>Sub Elements</em>}'
	 * @return the meta object for the containment reference list '<em>Sub Elements</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getSubElements()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EReference getDataExchangeElement_SubElements();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getParentElement <em>Parent Element</em>}'
	 * @return the meta object for the container reference '<em>Parent Element</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getParentElement()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EReference getDataExchangeElement_ParentElement();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataType
	 * <em>Data Type</em>}'
	 * @return the meta object for the reference '<em>Data Type</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataType()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EReference getDataExchangeElement_DataType();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMappingObject
	 * <em>Mapping Object</em>}'
	 * @return the meta object for the reference '<em>Mapping Object</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMappingObject()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EReference getDataExchangeElement_MappingObject();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#isContainer
	 * <em>Container</em>}'
	 * @return the meta object for the attribute '<em>Container</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#isContainer()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EAttribute getDataExchangeElement_Container();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#isDisableExternalMapping <em>Disable External
	 * Mapping</em>}'
	 * @return the meta object for the attribute '<em>Disable External Mapping</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#isDisableExternalMapping()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EAttribute getDataExchangeElement_DisableExternalMapping();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#isUsedForCustomQuery <em>Used For Custom Query</em>}'
	 * @return the meta object for the attribute '<em>Used For Custom Query</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#isUsedForCustomQuery()
	 * @see #getDataExchangeElement()
	 * @generated
	 */
	EAttribute getDataExchangeElement_UsedForCustomQuery();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute <em>Data Exchange
	 * Attribute</em>}'
	 * @return the meta object for class '<em>Data Exchange Attribute</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute
	 * @generated
	 */
	EClass getDataExchangeAttribute();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getValueListEntries <em>Value List Entries</em>}'
	 * @return the meta object for the containment reference list '<em>Value List Entries</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getValueListEntries()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EReference getDataExchangeAttribute_ValueListEntries();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getName
	 * <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getName()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EAttribute getDataExchangeAttribute_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isVisible
	 * <em>Visible</em>}'
	 * @return the meta object for the attribute '<em>Visible</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isVisible()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EAttribute getDataExchangeAttribute_Visible();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isOptional
	 * <em>Optional</em>}'
	 * @return the meta object for the attribute '<em>Optional</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isOptional()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EAttribute getDataExchangeAttribute_Optional();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getAttributeOrder <em>Attribute Order</em>}'
	 * @return the meta object for the attribute '<em>Attribute Order</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getAttributeOrder()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EAttribute getDataExchangeAttribute_AttributeOrder();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isReadonly
	 * <em>Readonly</em>}'
	 * @return the meta object for the attribute '<em>Readonly</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isReadonly()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EAttribute getDataExchangeAttribute_Readonly();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getElement <em>Element</em>}'
	 * @return the meta object for the container reference '<em>Element</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getElement()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EReference getDataExchangeAttribute_Element();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getMappingAttribute <em>Mapping Attribute</em>}'
	 * @return the meta object for the reference '<em>Mapping Attribute</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getMappingAttribute()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EReference getDataExchangeAttribute_MappingAttribute();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getDataType
	 * <em>Data Type</em>}'
	 * @return the meta object for the reference '<em>Data Type</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getDataType()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EReference getDataExchangeAttribute_DataType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getFormat
	 * <em>Format</em>}'
	 * @return the meta object for the attribute '<em>Format</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getFormat()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EAttribute getDataExchangeAttribute_Format();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isDisableExternalMapping <em>Disable External
	 * Mapping</em>}'
	 * @return the meta object for the attribute '<em>Disable External Mapping</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isDisableExternalMapping()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EAttribute getDataExchangeAttribute_DisableExternalMapping();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isUsedForCustomQuery <em>Used For Custom Query</em>}'
	 * @return the meta object for the attribute '<em>Used For Custom Query</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isUsedForCustomQuery()
	 * @see #getDataExchangeAttribute()
	 * @generated
	 */
	EAttribute getDataExchangeAttribute_UsedForCustomQuery();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.ValueListEntry <em>Value List Entry</em>}'
	 * @return the meta object for class '<em>Value List Entry</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ValueListEntry
	 * @generated
	 */
	EClass getValueListEntry();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.ValueListEntry#getItemText <em>Item
	 * Text</em>}'
	 * @return the meta object for the attribute '<em>Item Text</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ValueListEntry#getItemText()
	 * @see #getValueListEntry()
	 * @generated
	 */
	EAttribute getValueListEntry_ItemText();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject <em>Mapping
	 * Object</em>}'
	 * @return the meta object for class '<em>Mapping Object</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject
	 * @generated
	 */
	EClass getExchangeMappingObject();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#getAttributes <em>Attributes</em>}'
	 * @return the meta object for the containment reference list '<em>Attributes</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#getAttributes()
	 * @see #getExchangeMappingObject()
	 * @generated
	 */
	EReference getExchangeMappingObject_Attributes();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isDeleteAllItems
	 * <em>Delete All Items</em>}'
	 * @return the meta object for the attribute '<em>Delete All Items</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isDeleteAllItems()
	 * @see #getExchangeMappingObject()
	 * @generated
	 */
	EAttribute getExchangeMappingObject_DeleteAllItems();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isAddNewItems
	 * <em>Add New Items</em>}'
	 * @return the meta object for the attribute '<em>Add New Items</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isAddNewItems()
	 * @see #getExchangeMappingObject()
	 * @generated
	 */
	EAttribute getExchangeMappingObject_AddNewItems();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isUpdateExistingItems <em>Update Existing Items</em>}'
	 * @return the meta object for the attribute '<em>Update Existing Items</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingObject#isUpdateExistingItems()
	 * @see #getExchangeMappingObject()
	 * @generated
	 */
	EAttribute getExchangeMappingObject_UpdateExistingItems();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute <em>Mapping
	 * Attribute</em>}'
	 * @return the meta object for class '<em>Mapping Attribute</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute
	 * @generated
	 */
	EClass getExchangeMappingAttribute();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getExchangeMappingObject <em>Exchange Mapping
	 * Object</em>}'
	 * @return the meta object for the container reference '<em>Exchange Mapping Object</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getExchangeMappingObject()
	 * @see #getExchangeMappingAttribute()
	 * @generated
	 */
	EReference getExchangeMappingAttribute_ExchangeMappingObject();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isInsertable
	 * <em>Insertable</em>}'
	 * @return the meta object for the attribute '<em>Insertable</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isInsertable()
	 * @see #getExchangeMappingAttribute()
	 * @generated
	 */
	EAttribute getExchangeMappingAttribute_Insertable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isUpdatable
	 * <em>Updatable</em>}'
	 * @return the meta object for the attribute '<em>Updatable</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isUpdatable()
	 * @see #getExchangeMappingAttribute()
	 * @generated
	 */
	EAttribute getExchangeMappingAttribute_Updatable();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getSelectionListStatement <em>Selection List
	 * Statement</em>}'
	 * @return the meta object for the attribute '<em>Selection List Statement</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#getSelectionListStatement()
	 * @see #getExchangeMappingAttribute()
	 * @generated
	 */
	EAttribute getExchangeMappingAttribute_SelectionListStatement();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isDeleteAllItems <em>Delete All Items</em>}'
	 * @return the meta object for the attribute '<em>Delete All Items</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isDeleteAllItems()
	 * @see #getExchangeMappingAttribute()
	 * @generated
	 */
	EAttribute getExchangeMappingAttribute_DeleteAllItems();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isAddNewItems
	 * <em>Add New Items</em>}'
	 * @return the meta object for the attribute '<em>Add New Items</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isAddNewItems()
	 * @see #getExchangeMappingAttribute()
	 * @generated
	 */
	EAttribute getExchangeMappingAttribute_AddNewItems();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isJoinAttribute <em>Join Attribute</em>}'
	 * @return the meta object for the attribute '<em>Join Attribute</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isJoinAttribute()
	 * @see #getExchangeMappingAttribute()
	 * @generated
	 */
	EAttribute getExchangeMappingAttribute_JoinAttribute();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isUpdateExistingItems <em>Update Existing Items</em>}'
	 * @return the meta object for the attribute '<em>Update Existing Items</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute#isUpdateExistingItems()
	 * @see #getExchangeMappingAttribute()
	 * @generated
	 */
	EAttribute getExchangeMappingAttribute_UpdateExistingItems();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter <em>Filter Method
	 * Parameter</em>}'
	 * @return the meta object for class '<em>Filter Method Parameter</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter
	 * @generated
	 */
	EClass getFilterMethodParameter();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getDomainAttribute <em>Domain Attribute</em>}'
	 * @return the meta object for the reference '<em>Domain Attribute</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getDomainAttribute()
	 * @see #getFilterMethodParameter()
	 * @generated
	 */
	EReference getFilterMethodParameter_DomainAttribute();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getAssociation
	 * <em>Association</em>}'
	 * @return the meta object for the reference '<em>Association</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getAssociation()
	 * @see #getFilterMethodParameter()
	 * @generated
	 */
	EReference getFilterMethodParameter_Association();

	/**
	 * Return the meta object for the reference list
	 * '{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getAssociationList <em>Association List</em>}'
	 * @return the meta object for the reference list '<em>Association List</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getAssociationList()
	 * @see #getFilterMethodParameter()
	 * @generated
	 */
	EReference getFilterMethodParameter_AssociationList();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getOperator
	 * <em>Operator</em>}'
	 * @return the meta object for the attribute '<em>Operator</em>'
	 * @see net.codecadenza.eclipse.model.exchange.FilterMethodParameter#getOperator()
	 * @see #getFilterMethodParameter()
	 * @generated
	 */
	EAttribute getFilterMethodParameter_Operator();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.AssociationController <em>Association
	 * Controller</em>}'
	 * @return the meta object for class '<em>Association Controller</em>'
	 * @see net.codecadenza.eclipse.model.exchange.AssociationController
	 * @generated
	 */
	EClass getAssociationController();

	/**
	 * Return the meta object for the reference list
	 * '{@link net.codecadenza.eclipse.model.exchange.AssociationController#getQueryAttributes <em>Query Attributes</em>}'
	 * @return the meta object for the reference list '<em>Query Attributes</em>'
	 * @see net.codecadenza.eclipse.model.exchange.AssociationController#getQueryAttributes()
	 * @see #getAssociationController()
	 * @generated
	 */
	EReference getAssociationController_QueryAttributes();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.exchange.AssociationController#getAssociation
	 * <em>Association</em>}'
	 * @return the meta object for the reference '<em>Association</em>'
	 * @see net.codecadenza.eclipse.model.exchange.AssociationController#getAssociation()
	 * @see #getAssociationController()
	 * @generated
	 */
	EReference getAssociationController_Association();

	/**
	 * Return the meta object for the reference list
	 * '{@link net.codecadenza.eclipse.model.exchange.AssociationController#getPersistAttributes <em>Persist Attributes</em>}'
	 * @return the meta object for the reference list '<em>Persist Attributes</em>'
	 * @see net.codecadenza.eclipse.model.exchange.AssociationController#getPersistAttributes()
	 * @see #getAssociationController()
	 * @generated
	 */
	EReference getAssociationController_PersistAttributes();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.exchange.DirectExchangeMode <em>Direct Exchange
	 * Mode</em>}'
	 * @return the meta object for class '<em>Direct Exchange Mode</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DirectExchangeMode
	 * @generated
	 */
	EClass getDirectExchangeMode();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration <em>Data
	 * Exchange Method Type Enumeration</em>}'
	 * @return the meta object for enum '<em>Data Exchange Method Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration
	 * @generated
	 */
	EEnum getDataExchangeMethodTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration <em>Content Type
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Content Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration
	 * @generated
	 */
	EEnum getContentTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration <em>Parser
	 * Implementation Enumeration</em>}'
	 * @return the meta object for enum '<em>Parser Implementation Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration
	 * @generated
	 */
	EEnum getParserImplementationEnumeration();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	ExchangeFactory getExchangeFactory();

	/**
	 * Defines literals for the meta objects that represent
	 * <ul>
	 * <li>each class,</li>
	 * <li>each feature of each class,</li>
	 * <li>each enum,</li>
	 * <li>and each data type</li>
	 * </ul>
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeServiceBeanImpl <em>Data
		 * Exchange Service Bean</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeServiceBeanImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeServiceBean()
		 * @generated
		 */
		EClass DATA_EXCHANGE_SERVICE_BEAN = eINSTANCE.getDataExchangeServiceBean();

		/**
		 * The meta object literal for the '<em><b>Data Exchange Methods</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_SERVICE_BEAN__DATA_EXCHANGE_METHODS = eINSTANCE.getDataExchangeServiceBean_DataExchangeMethods();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl <em>Data Exchange
		 * Method</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeMethodImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeMethod()
		 * @generated
		 */
		EClass DATA_EXCHANGE_METHOD = eINSTANCE.getDataExchangeMethod();

		/**
		 * The meta object literal for the '<em><b>Data Exchange Service Bean</b></em>' container reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_METHOD__DATA_EXCHANGE_SERVICE_BEAN = eINSTANCE.getDataExchangeMethod_DataExchangeServiceBean();

		/**
		 * The meta object literal for the '<em><b>Process Single Object</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__PROCESS_SINGLE_OBJECT = eINSTANCE.getDataExchangeMethod_ProcessSingleObject();

		/**
		 * The meta object literal for the '<em><b>Joined Import Method</b></em>' reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_METHOD__JOINED_IMPORT_METHOD = eINSTANCE.getDataExchangeMethod_JoinedImportMethod();

		/**
		 * The meta object literal for the '<em><b>Quote Character</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__QUOTE_CHARACTER = eINSTANCE.getDataExchangeMethod_QuoteCharacter();

		/**
		 * The meta object literal for the '<em><b>Record Separator</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__RECORD_SEPARATOR = eINSTANCE.getDataExchangeMethod_RecordSeparator();

		/**
		 * The meta object literal for the '<em><b>Comment Character</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__COMMENT_CHARACTER = eINSTANCE.getDataExchangeMethod_CommentCharacter();

		/**
		 * The meta object literal for the '<em><b>Delimiter</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__DELIMITER = eINSTANCE.getDataExchangeMethod_Delimiter();

		/**
		 * The meta object literal for the '<em><b>Default Date Format</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__DEFAULT_DATE_FORMAT = eINSTANCE.getDataExchangeMethod_DefaultDateFormat();

		/**
		 * The meta object literal for the '<em><b>Default Date Time Format</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__DEFAULT_DATE_TIME_FORMAT = eINSTANCE.getDataExchangeMethod_DefaultDateTimeFormat();

		/**
		 * The meta object literal for the '<em><b>Default Number Format</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__DEFAULT_NUMBER_FORMAT = eINSTANCE.getDataExchangeMethod_DefaultNumberFormat();

		/**
		 * The meta object literal for the '<em><b>Association Controllers</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_METHOD__ASSOCIATION_CONTROLLERS = eINSTANCE.getDataExchangeMethod_AssociationControllers();

		/**
		 * The meta object literal for the '<em><b>Schema File Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__SCHEMA_FILE_NAME = eINSTANCE.getDataExchangeMethod_SchemaFileName();

		/**
		 * The meta object literal for the '<em><b>Exchange Mode</b></em>' containment reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_METHOD__EXCHANGE_MODE = eINSTANCE.getDataExchangeMethod_ExchangeMode();

		/**
		 * The meta object literal for the '<em><b>Root Element</b></em>' containment reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_METHOD__ROOT_ELEMENT = eINSTANCE.getDataExchangeMethod_RootElement();

		/**
		 * The meta object literal for the '<em><b>Content Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__CONTENT_TYPE = eINSTANCE.getDataExchangeMethod_ContentType();

		/**
		 * The meta object literal for the '<em><b>Method Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__METHOD_TYPE = eINSTANCE.getDataExchangeMethod_MethodType();

		/**
		 * The meta object literal for the '<em><b>Parser</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__PARSER = eINSTANCE.getDataExchangeMethod_Parser();

		/**
		 * The meta object literal for the '<em><b>Perform Validation</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__PERFORM_VALIDATION = eINSTANCE.getDataExchangeMethod_PerformValidation();

		/**
		 * The meta object literal for the '<em><b>Charset</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__CHARSET = eINSTANCE.getDataExchangeMethod_Charset();

		/**
		 * The meta object literal for the '<em><b>Format Output</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_METHOD__FORMAT_OUTPUT = eINSTANCE.getDataExchangeMethod_FormatOutput();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl <em>Data Exchange
		 * Mode</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeModeImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeMode()
		 * @generated
		 */
		EClass DATA_EXCHANGE_MODE = eINSTANCE.getDataExchangeMode();

		/**
		 * The meta object literal for the '<em><b>Max Objects To Be Processed</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_MODE__MAX_OBJECTS_TO_BE_PROCESSED = eINSTANCE.getDataExchangeMode_MaxObjectsToBeProcessed();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.StringExchangeModeImpl <em>String
		 * Exchange Mode</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.StringExchangeModeImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getStringExchangeMode()
		 * @generated
		 */
		EClass STRING_EXCHANGE_MODE = eINSTANCE.getStringExchangeMode();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.FileExchangeModeImpl <em>File Exchange
		 * Mode</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.FileExchangeModeImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getFileExchangeMode()
		 * @generated
		 */
		EClass FILE_EXCHANGE_MODE = eINSTANCE.getFileExchangeMode();

		/**
		 * The meta object literal for the '<em><b>Path</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FILE_EXCHANGE_MODE__PATH = eINSTANCE.getFileExchangeMode_Path();

		/**
		 * The meta object literal for the '<em><b>File Name Pattern</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FILE_EXCHANGE_MODE__FILE_NAME_PATTERN = eINSTANCE.getFileExchangeMode_FileNamePattern();

		/**
		 * The meta object literal for the '<em><b>Block Size</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FILE_EXCHANGE_MODE__BLOCK_SIZE = eINSTANCE.getFileExchangeMode_BlockSize();

		/**
		 * The meta object literal for the '<em><b>New Transaction Per File</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FILE_EXCHANGE_MODE__NEW_TRANSACTION_PER_FILE = eINSTANCE.getFileExchangeMode_NewTransactionPerFile();

		/**
		 * The meta object literal for the '<em><b>Delete After Import</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FILE_EXCHANGE_MODE__DELETE_AFTER_IMPORT = eINSTANCE.getFileExchangeMode_DeleteAfterImport();

		/**
		 * The meta object literal for the '<em><b>Target Path After Import</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FILE_EXCHANGE_MODE__TARGET_PATH_AFTER_IMPORT = eINSTANCE.getFileExchangeMode_TargetPathAfterImport();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl <em>Data
		 * Exchange Element</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeElementImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeElement()
		 * @generated
		 */
		EClass DATA_EXCHANGE_ELEMENT = eINSTANCE.getDataExchangeElement();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ELEMENT__NAME = eINSTANCE.getDataExchangeElement_Name();

		/**
		 * The meta object literal for the '<em><b>Min Occurrences</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ELEMENT__MIN_OCCURRENCES = eINSTANCE.getDataExchangeElement_MinOccurrences();

		/**
		 * The meta object literal for the '<em><b>Max Occurrences</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ELEMENT__MAX_OCCURRENCES = eINSTANCE.getDataExchangeElement_MaxOccurrences();

		/**
		 * The meta object literal for the '<em><b>Element Order</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ELEMENT__ELEMENT_ORDER = eINSTANCE.getDataExchangeElement_ElementOrder();

		/**
		 * The meta object literal for the '<em><b>Type Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ELEMENT__TYPE_NAME = eINSTANCE.getDataExchangeElement_TypeName();

		/**
		 * The meta object literal for the '<em><b>Wrapper Element Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ELEMENT__WRAPPER_ELEMENT_NAME = eINSTANCE.getDataExchangeElement_WrapperElementName();

		/**
		 * The meta object literal for the '<em><b>Data Exchange Method</b></em>' container reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ELEMENT__DATA_EXCHANGE_METHOD = eINSTANCE.getDataExchangeElement_DataExchangeMethod();

		/**
		 * The meta object literal for the '<em><b>Value List Entries</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ELEMENT__VALUE_LIST_ENTRIES = eINSTANCE.getDataExchangeElement_ValueListEntries();

		/**
		 * The meta object literal for the '<em><b>Attributes</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ELEMENT__ATTRIBUTES = eINSTANCE.getDataExchangeElement_Attributes();

		/**
		 * The meta object literal for the '<em><b>Mapping Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ELEMENT__MAPPING_ATTRIBUTE = eINSTANCE.getDataExchangeElement_MappingAttribute();

		/**
		 * The meta object literal for the '<em><b>Sub Elements</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ELEMENT__SUB_ELEMENTS = eINSTANCE.getDataExchangeElement_SubElements();

		/**
		 * The meta object literal for the '<em><b>Parent Element</b></em>' container reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ELEMENT__PARENT_ELEMENT = eINSTANCE.getDataExchangeElement_ParentElement();

		/**
		 * The meta object literal for the '<em><b>Data Type</b></em>' reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ELEMENT__DATA_TYPE = eINSTANCE.getDataExchangeElement_DataType();

		/**
		 * The meta object literal for the '<em><b>Mapping Object</b></em>' reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ELEMENT__MAPPING_OBJECT = eINSTANCE.getDataExchangeElement_MappingObject();

		/**
		 * The meta object literal for the '<em><b>Container</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ELEMENT__CONTAINER = eINSTANCE.getDataExchangeElement_Container();

		/**
		 * The meta object literal for the '<em><b>Disable External Mapping</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ELEMENT__DISABLE_EXTERNAL_MAPPING = eINSTANCE.getDataExchangeElement_DisableExternalMapping();

		/**
		 * The meta object literal for the '<em><b>Used For Custom Query</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ELEMENT__USED_FOR_CUSTOM_QUERY = eINSTANCE.getDataExchangeElement_UsedForCustomQuery();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl <em>Data
		 * Exchange Attribute</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.DataExchangeAttributeImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeAttribute()
		 * @generated
		 */
		EClass DATA_EXCHANGE_ATTRIBUTE = eINSTANCE.getDataExchangeAttribute();

		/**
		 * The meta object literal for the '<em><b>Value List Entries</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ATTRIBUTE__VALUE_LIST_ENTRIES = eINSTANCE.getDataExchangeAttribute_ValueListEntries();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ATTRIBUTE__NAME = eINSTANCE.getDataExchangeAttribute_Name();

		/**
		 * The meta object literal for the '<em><b>Visible</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ATTRIBUTE__VISIBLE = eINSTANCE.getDataExchangeAttribute_Visible();

		/**
		 * The meta object literal for the '<em><b>Optional</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ATTRIBUTE__OPTIONAL = eINSTANCE.getDataExchangeAttribute_Optional();

		/**
		 * The meta object literal for the '<em><b>Attribute Order</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ATTRIBUTE__ATTRIBUTE_ORDER = eINSTANCE.getDataExchangeAttribute_AttributeOrder();

		/**
		 * The meta object literal for the '<em><b>Readonly</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ATTRIBUTE__READONLY = eINSTANCE.getDataExchangeAttribute_Readonly();

		/**
		 * The meta object literal for the '<em><b>Element</b></em>' container reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ATTRIBUTE__ELEMENT = eINSTANCE.getDataExchangeAttribute_Element();

		/**
		 * The meta object literal for the '<em><b>Mapping Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ATTRIBUTE__MAPPING_ATTRIBUTE = eINSTANCE.getDataExchangeAttribute_MappingAttribute();

		/**
		 * The meta object literal for the '<em><b>Data Type</b></em>' reference feature
		 * @generated
		 */
		EReference DATA_EXCHANGE_ATTRIBUTE__DATA_TYPE = eINSTANCE.getDataExchangeAttribute_DataType();

		/**
		 * The meta object literal for the '<em><b>Format</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ATTRIBUTE__FORMAT = eINSTANCE.getDataExchangeAttribute_Format();

		/**
		 * The meta object literal for the '<em><b>Disable External Mapping</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ATTRIBUTE__DISABLE_EXTERNAL_MAPPING = eINSTANCE.getDataExchangeAttribute_DisableExternalMapping();

		/**
		 * The meta object literal for the '<em><b>Used For Custom Query</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DATA_EXCHANGE_ATTRIBUTE__USED_FOR_CUSTOM_QUERY = eINSTANCE.getDataExchangeAttribute_UsedForCustomQuery();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.ValueListEntryImpl <em>Value List
		 * Entry</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.ValueListEntryImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getValueListEntry()
		 * @generated
		 */
		EClass VALUE_LIST_ENTRY = eINSTANCE.getValueListEntry();

		/**
		 * The meta object literal for the '<em><b>Item Text</b></em>' attribute feature
		 * @generated
		 */
		EAttribute VALUE_LIST_ENTRY__ITEM_TEXT = eINSTANCE.getValueListEntry_ItemText();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingObjectImpl <em>Mapping
		 * Object</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingObjectImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getExchangeMappingObject()
		 * @generated
		 */
		EClass EXCHANGE_MAPPING_OBJECT = eINSTANCE.getExchangeMappingObject();

		/**
		 * The meta object literal for the '<em><b>Attributes</b></em>' containment reference list feature
		 * @generated
		 */
		EReference EXCHANGE_MAPPING_OBJECT__ATTRIBUTES = eINSTANCE.getExchangeMappingObject_Attributes();

		/**
		 * The meta object literal for the '<em><b>Delete All Items</b></em>' attribute feature
		 * @generated
		 */
		EAttribute EXCHANGE_MAPPING_OBJECT__DELETE_ALL_ITEMS = eINSTANCE.getExchangeMappingObject_DeleteAllItems();

		/**
		 * The meta object literal for the '<em><b>Add New Items</b></em>' attribute feature
		 * @generated
		 */
		EAttribute EXCHANGE_MAPPING_OBJECT__ADD_NEW_ITEMS = eINSTANCE.getExchangeMappingObject_AddNewItems();

		/**
		 * The meta object literal for the '<em><b>Update Existing Items</b></em>' attribute feature
		 * @generated
		 */
		EAttribute EXCHANGE_MAPPING_OBJECT__UPDATE_EXISTING_ITEMS = eINSTANCE.getExchangeMappingObject_UpdateExistingItems();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl <em>Mapping
		 * Attribute</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangeMappingAttributeImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getExchangeMappingAttribute()
		 * @generated
		 */
		EClass EXCHANGE_MAPPING_ATTRIBUTE = eINSTANCE.getExchangeMappingAttribute();

		/**
		 * The meta object literal for the '<em><b>Exchange Mapping Object</b></em>' container reference feature
		 * @generated
		 */
		EReference EXCHANGE_MAPPING_ATTRIBUTE__EXCHANGE_MAPPING_OBJECT = eINSTANCE
				.getExchangeMappingAttribute_ExchangeMappingObject();

		/**
		 * The meta object literal for the '<em><b>Insertable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute EXCHANGE_MAPPING_ATTRIBUTE__INSERTABLE = eINSTANCE.getExchangeMappingAttribute_Insertable();

		/**
		 * The meta object literal for the '<em><b>Updatable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute EXCHANGE_MAPPING_ATTRIBUTE__UPDATABLE = eINSTANCE.getExchangeMappingAttribute_Updatable();

		/**
		 * The meta object literal for the '<em><b>Selection List Statement</b></em>' attribute feature
		 * @generated
		 */
		EAttribute EXCHANGE_MAPPING_ATTRIBUTE__SELECTION_LIST_STATEMENT = eINSTANCE
				.getExchangeMappingAttribute_SelectionListStatement();

		/**
		 * The meta object literal for the '<em><b>Delete All Items</b></em>' attribute feature
		 * @generated
		 */
		EAttribute EXCHANGE_MAPPING_ATTRIBUTE__DELETE_ALL_ITEMS = eINSTANCE.getExchangeMappingAttribute_DeleteAllItems();

		/**
		 * The meta object literal for the '<em><b>Add New Items</b></em>' attribute feature
		 * @generated
		 */
		EAttribute EXCHANGE_MAPPING_ATTRIBUTE__ADD_NEW_ITEMS = eINSTANCE.getExchangeMappingAttribute_AddNewItems();

		/**
		 * The meta object literal for the '<em><b>Join Attribute</b></em>' attribute feature
		 * @generated
		 */
		EAttribute EXCHANGE_MAPPING_ATTRIBUTE__JOIN_ATTRIBUTE = eINSTANCE.getExchangeMappingAttribute_JoinAttribute();

		/**
		 * The meta object literal for the '<em><b>Update Existing Items</b></em>' attribute feature
		 * @generated
		 */
		EAttribute EXCHANGE_MAPPING_ATTRIBUTE__UPDATE_EXISTING_ITEMS = eINSTANCE.getExchangeMappingAttribute_UpdateExistingItems();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.FilterMethodParameterImpl <em>Filter
		 * Method Parameter</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.FilterMethodParameterImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getFilterMethodParameter()
		 * @generated
		 */
		EClass FILTER_METHOD_PARAMETER = eINSTANCE.getFilterMethodParameter();

		/**
		 * The meta object literal for the '<em><b>Domain Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference FILTER_METHOD_PARAMETER__DOMAIN_ATTRIBUTE = eINSTANCE.getFilterMethodParameter_DomainAttribute();

		/**
		 * The meta object literal for the '<em><b>Association</b></em>' reference feature
		 * @generated
		 */
		EReference FILTER_METHOD_PARAMETER__ASSOCIATION = eINSTANCE.getFilterMethodParameter_Association();

		/**
		 * The meta object literal for the '<em><b>Association List</b></em>' reference list feature
		 * @generated
		 */
		EReference FILTER_METHOD_PARAMETER__ASSOCIATION_LIST = eINSTANCE.getFilterMethodParameter_AssociationList();

		/**
		 * The meta object literal for the '<em><b>Operator</b></em>' attribute feature
		 * @generated
		 */
		EAttribute FILTER_METHOD_PARAMETER__OPERATOR = eINSTANCE.getFilterMethodParameter_Operator();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.AssociationControllerImpl
		 * <em>Association Controller</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.AssociationControllerImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getAssociationController()
		 * @generated
		 */
		EClass ASSOCIATION_CONTROLLER = eINSTANCE.getAssociationController();

		/**
		 * The meta object literal for the '<em><b>Query Attributes</b></em>' reference list feature
		 * @generated
		 */
		EReference ASSOCIATION_CONTROLLER__QUERY_ATTRIBUTES = eINSTANCE.getAssociationController_QueryAttributes();

		/**
		 * The meta object literal for the '<em><b>Association</b></em>' reference feature
		 * @generated
		 */
		EReference ASSOCIATION_CONTROLLER__ASSOCIATION = eINSTANCE.getAssociationController_Association();

		/**
		 * The meta object literal for the '<em><b>Persist Attributes</b></em>' reference list feature
		 * @generated
		 */
		EReference ASSOCIATION_CONTROLLER__PERSIST_ATTRIBUTES = eINSTANCE.getAssociationController_PersistAttributes();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.impl.DirectExchangeModeImpl <em>Direct
		 * Exchange Mode</em>}' class
		 * @see net.codecadenza.eclipse.model.exchange.impl.DirectExchangeModeImpl
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDirectExchangeMode()
		 * @generated
		 */
		EClass DIRECT_EXCHANGE_MODE = eINSTANCE.getDirectExchangeMode();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration <em>Data
		 * Exchange Method Type Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getDataExchangeMethodTypeEnumeration()
		 * @generated
		 */
		EEnum DATA_EXCHANGE_METHOD_TYPE_ENUMERATION = eINSTANCE.getDataExchangeMethodTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration <em>Content Type
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getContentTypeEnumeration()
		 * @generated
		 */
		EEnum CONTENT_TYPE_ENUMERATION = eINSTANCE.getContentTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration <em>Parser
		 * Implementation Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.exchange.ParserImplementationEnumeration
		 * @see net.codecadenza.eclipse.model.exchange.impl.ExchangePackageImpl#getParserImplementationEnumeration()
		 * @generated
		 */
		EEnum PARSER_IMPLEMENTATION_ENUMERATION = eINSTANCE.getParserImplementationEnumeration();

	}

}
