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
package net.codecadenza.eclipse.model.domain;

import net.codecadenza.eclipse.model.java.JavaPackage;
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
 * @see net.codecadenza.eclipse.model.domain.DomainFactory
 * @model kind="package"
 * @generated
 */
public interface DomainPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "domain";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/domain.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.domain";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	DomainPackage eINSTANCE = net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl <em>Abstract
	 * Domain Association</em>}' class
	 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getAbstractDomainAssociation()
	 * @generated
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Owner</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__OWNER = 1;

	/**
	 * The feature ID for the '<em><b>Cascade Persist</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST = 2;

	/**
	 * The feature ID for the '<em><b>Cascade Merge</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE = 3;

	/**
	 * The feature ID for the '<em><b>Cascade Remove</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE = 4;

	/**
	 * The feature ID for the '<em><b>Cascade Refresh</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH = 5;

	/**
	 * The feature ID for the '<em><b>Fetch Type Eager</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER = 6;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT = 7;

	/**
	 * The feature ID for the '<em><b>Target</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__TARGET = 8;

	/**
	 * The feature ID for the '<em><b>Tag</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__TAG = 9;

	/**
	 * The feature ID for the '<em><b>Internal Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT = 10;

	/**
	 * The feature ID for the '<em><b>User Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT = 11;

	/**
	 * The feature ID for the '<em><b>Reverse Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION = 12;

	/**
	 * The number of structural features of the '<em>Abstract Domain Association</em>' class
	 * @generated
	 * @ordered
	 */
	int ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT = 13;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl <em>Object</em>}' class
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainObject()
	 * @generated
	 */
	int DOMAIN_OBJECT = 1;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__NAME = JavaPackage.JAVA_TYPE__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__COMMENT = JavaPackage.JAVA_TYPE__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__MAPPABLE = JavaPackage.JAVA_TYPE__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__PRIMITIVE = JavaPackage.JAVA_TYPE__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__NAMESPACE = JavaPackage.JAVA_TYPE__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Label</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__LABEL = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Name Plural</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__NAME_PLURAL = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Label Plural</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__LABEL_PLURAL = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Discriminator Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__DISCRIMINATOR_VALUE = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 3;

	/**
	 * The feature ID for the '<em><b>Discriminator Column Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__DISCRIMINATOR_COLUMN_TYPE = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 4;

	/**
	 * The feature ID for the '<em><b>Inheritance Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__INHERITANCE_TYPE = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 5;

	/**
	 * The feature ID for the '<em><b>Property Access</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__PROPERTY_ACCESS = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 6;

	/**
	 * The feature ID for the '<em><b>Abstract</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__ABSTRACT = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 7;

	/**
	 * The feature ID for the '<em><b>Mapped Super Class</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__MAPPED_SUPER_CLASS = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 8;

	/**
	 * The feature ID for the '<em><b>Parent</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__PARENT = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 9;

	/**
	 * The feature ID for the '<em><b>ID Generator</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__ID_GENERATOR = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 10;

	/**
	 * The feature ID for the '<em><b>Inheritance</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__INHERITANCE = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 11;

	/**
	 * The feature ID for the '<em><b>Attributes</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__ATTRIBUTES = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 12;

	/**
	 * The feature ID for the '<em><b>Associations</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__ASSOCIATIONS = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 13;

	/**
	 * The feature ID for the '<em><b>Enum Associations</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__ENUM_ASSOCIATIONS = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 14;

	/**
	 * The feature ID for the '<em><b>Target Inheritances</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__TARGET_INHERITANCES = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 15;

	/**
	 * The feature ID for the '<em><b>Discriminator Column</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__DISCRIMINATOR_COLUMN = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 16;

	/**
	 * The feature ID for the '<em><b>Database Table</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__DATABASE_TABLE = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 17;

	/**
	 * The feature ID for the '<em><b>Tag</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT__TAG = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 18;

	/**
	 * The number of structural features of the '<em>Object</em>' class
	 * @generated
	 * @ordered
	 */
	int DOMAIN_OBJECT_FEATURE_COUNT = JavaPackage.JAVA_TYPE_FEATURE_COUNT + 19;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl <em>Domain Attribute</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainAttribute()
	 * @generated
	 */
	int DOMAIN_ATTRIBUTE = 2;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Pk</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__PK = 1;

	/**
	 * The feature ID for the '<em><b>Label</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__LABEL = 2;

	/**
	 * The feature ID for the '<em><b>Label Plural</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__LABEL_PLURAL = 3;

	/**
	 * The feature ID for the '<em><b>Persistent</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__PERSISTENT = 4;

	/**
	 * The feature ID for the '<em><b>Fetch Type Eager</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__FETCH_TYPE_EAGER = 5;

	/**
	 * The feature ID for the '<em><b>Insertable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__INSERTABLE = 6;

	/**
	 * The feature ID for the '<em><b>Updatable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__UPDATABLE = 7;

	/**
	 * The feature ID for the '<em><b>Track Version</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__TRACK_VERSION = 8;

	/**
	 * The feature ID for the '<em><b>Set Date On Persist</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__SET_DATE_ON_PERSIST = 9;

	/**
	 * The feature ID for the '<em><b>Set Date On Update</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__SET_DATE_ON_UPDATE = 10;

	/**
	 * The feature ID for the '<em><b>Display Attribute</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__DISPLAY_ATTRIBUTE = 11;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__DOMAIN_OBJECT = 12;

	/**
	 * The feature ID for the '<em><b>Domain Attribute Validator</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR = 13;

	/**
	 * The feature ID for the '<em><b>Temporal Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__TEMPORAL_TYPE = 14;

	/**
	 * The feature ID for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__JAVA_TYPE = 15;

	/**
	 * The feature ID for the '<em><b>Column</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__COLUMN = 16;

	/**
	 * The feature ID for the '<em><b>Tag</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__TAG = 17;

	/**
	 * The feature ID for the '<em><b>Lob</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__LOB = 18;

	/**
	 * The feature ID for the '<em><b>Internal Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__INTERNAL_COMMENT = 19;

	/**
	 * The feature ID for the '<em><b>User Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__USER_COMMENT = 20;

	/**
	 * The feature ID for the '<em><b>Remove Whitespace Characters</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__REMOVE_WHITESPACE_CHARACTERS = 21;

	/**
	 * The feature ID for the '<em><b>Convert To Upper Case</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__CONVERT_TO_UPPER_CASE = 22;

	/**
	 * The feature ID for the '<em><b>Convert To Lower Case</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__CONVERT_TO_LOWER_CASE = 23;

	/**
	 * The feature ID for the '<em><b>Collection Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__COLLECTION_TYPE = 24;

	/**
	 * The feature ID for the '<em><b>Collection Mapping Strategy</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE__COLLECTION_MAPPING_STRATEGY = 25;

	/**
	 * The number of structural features of the '<em>Domain Attribute</em>' class
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE_FEATURE_COUNT = 26;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl <em>Attribute
	 * Validator</em>}' class
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainAttributeValidator()
	 * @generated
	 */
	int DOMAIN_ATTRIBUTE_VALIDATOR = 3;

	/**
	 * The feature ID for the '<em><b>Future Date</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE_VALIDATOR__FUTURE_DATE = 0;

	/**
	 * The feature ID for the '<em><b>Past Date</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE_VALIDATOR__PAST_DATE = 1;

	/**
	 * The feature ID for the '<em><b>Max Length</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE_VALIDATOR__MAX_LENGTH = 2;

	/**
	 * The feature ID for the '<em><b>Min Length</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE_VALIDATOR__MIN_LENGTH = 3;

	/**
	 * The feature ID for the '<em><b>Nullable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE_VALIDATOR__NULLABLE = 4;

	/**
	 * The feature ID for the '<em><b>Max Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE_VALIDATOR__MAX_VALUE = 5;

	/**
	 * The feature ID for the '<em><b>Min Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE_VALIDATOR__MIN_VALUE = 6;

	/**
	 * The feature ID for the '<em><b>Regular Expression</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE_VALIDATOR__REGULAR_EXPRESSION = 7;

	/**
	 * The number of structural features of the '<em>Attribute Validator</em>' class
	 * @generated
	 * @ordered
	 */
	int DOMAIN_ATTRIBUTE_VALIDATOR_FEATURE_COUNT = 8;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.DomainInheritanceImpl <em>Inheritance</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainInheritanceImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainInheritance()
	 * @generated
	 */
	int DOMAIN_INHERITANCE = 4;

	/**
	 * The feature ID for the '<em><b>Source</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_INHERITANCE__SOURCE = 0;

	/**
	 * The feature ID for the '<em><b>Target</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_INHERITANCE__TARGET = 1;

	/**
	 * The number of structural features of the '<em>Inheritance</em>' class
	 * @generated
	 * @ordered
	 */
	int DOMAIN_INHERITANCE_FEATURE_COUNT = 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.DomainNamespaceImpl <em>Namespace</em>}' class
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainNamespaceImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainNamespace()
	 * @generated
	 */
	int DOMAIN_NAMESPACE = 5;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int DOMAIN_NAMESPACE__NAME = JavaPackage.NAMESPACE__NAME;

	/**
	 * The feature ID for the '<em><b>Parent</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_NAMESPACE__PARENT = JavaPackage.NAMESPACE__PARENT;

	/**
	 * The feature ID for the '<em><b>Child Namespaces</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int DOMAIN_NAMESPACE__CHILD_NAMESPACES = JavaPackage.NAMESPACE__CHILD_NAMESPACES;

	/**
	 * The feature ID for the '<em><b>Java Types</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int DOMAIN_NAMESPACE__JAVA_TYPES = JavaPackage.NAMESPACE__JAVA_TYPES;

	/**
	 * The feature ID for the '<em><b>Project</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int DOMAIN_NAMESPACE__PROJECT = JavaPackage.NAMESPACE__PROJECT;

	/**
	 * The feature ID for the '<em><b>Domain Objects</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DOMAIN_NAMESPACE__DOMAIN_OBJECTS = JavaPackage.NAMESPACE_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Enumerations</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int DOMAIN_NAMESPACE__ENUMERATIONS = JavaPackage.NAMESPACE_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Namespace</em>' class
	 * @generated
	 * @ordered
	 */
	int DOMAIN_NAMESPACE_FEATURE_COUNT = JavaPackage.NAMESPACE_FEATURE_COUNT + 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.EnumAssociationImpl <em>Enum Association</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.domain.impl.EnumAssociationImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getEnumAssociation()
	 * @generated
	 */
	int ENUM_ASSOCIATION = 6;

	/**
	 * The feature ID for the '<em><b>Source</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int ENUM_ASSOCIATION__SOURCE = 0;

	/**
	 * The feature ID for the '<em><b>Target</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ENUM_ASSOCIATION__TARGET = 1;

	/**
	 * The feature ID for the '<em><b>Domain Attribute</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ENUM_ASSOCIATION__DOMAIN_ATTRIBUTE = 2;

	/**
	 * The number of structural features of the '<em>Enum Association</em>' class
	 * @generated
	 * @ordered
	 */
	int ENUM_ASSOCIATION_FEATURE_COUNT = 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.IDGeneratorImpl <em>ID Generator</em>}' class
	 * @see net.codecadenza.eclipse.model.domain.impl.IDGeneratorImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getIDGenerator()
	 * @generated
	 */
	int ID_GENERATOR = 7;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ID_GENERATOR__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Block Size</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ID_GENERATOR__BLOCK_SIZE = 1;

	/**
	 * The feature ID for the '<em><b>Initial Value</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ID_GENERATOR__INITIAL_VALUE = 2;

	/**
	 * The feature ID for the '<em><b>Generator Type</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ID_GENERATOR__GENERATOR_TYPE = 3;

	/**
	 * The number of structural features of the '<em>ID Generator</em>' class
	 * @generated
	 * @ordered
	 */
	int ID_GENERATOR_FEATURE_COUNT = 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.ManyToManyAssociationImpl <em>Many To Many
	 * Association</em>}' class
	 * @see net.codecadenza.eclipse.model.domain.impl.ManyToManyAssociationImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getManyToManyAssociation()
	 * @generated
	 */
	int MANY_TO_MANY_ASSOCIATION = 8;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__NAME = ABSTRACT_DOMAIN_ASSOCIATION__NAME;

	/**
	 * The feature ID for the '<em><b>Owner</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__OWNER = ABSTRACT_DOMAIN_ASSOCIATION__OWNER;

	/**
	 * The feature ID for the '<em><b>Cascade Persist</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__CASCADE_PERSIST = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST;

	/**
	 * The feature ID for the '<em><b>Cascade Merge</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__CASCADE_MERGE = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE;

	/**
	 * The feature ID for the '<em><b>Cascade Remove</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__CASCADE_REMOVE = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE;

	/**
	 * The feature ID for the '<em><b>Cascade Refresh</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__CASCADE_REFRESH = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH;

	/**
	 * The feature ID for the '<em><b>Fetch Type Eager</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__FETCH_TYPE_EAGER = ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__DOMAIN_OBJECT = ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Target</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__TARGET = ABSTRACT_DOMAIN_ASSOCIATION__TARGET;

	/**
	 * The feature ID for the '<em><b>Tag</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__TAG = ABSTRACT_DOMAIN_ASSOCIATION__TAG;

	/**
	 * The feature ID for the '<em><b>Internal Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__INTERNAL_COMMENT = ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT;

	/**
	 * The feature ID for the '<em><b>User Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__USER_COMMENT = ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT;

	/**
	 * The feature ID for the '<em><b>Reverse Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__REVERSE_ASSOCIATION = ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION;

	/**
	 * The feature ID for the '<em><b>Table</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION__TABLE = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Many To Many Association</em>' class
	 * @generated
	 * @ordered
	 */
	int MANY_TO_MANY_ASSOCIATION_FEATURE_COUNT = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.ManyToOneAssociationImpl <em>Many To One
	 * Association</em>}' class
	 * @see net.codecadenza.eclipse.model.domain.impl.ManyToOneAssociationImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getManyToOneAssociation()
	 * @generated
	 */
	int MANY_TO_ONE_ASSOCIATION = 9;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__NAME = ABSTRACT_DOMAIN_ASSOCIATION__NAME;

	/**
	 * The feature ID for the '<em><b>Owner</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__OWNER = ABSTRACT_DOMAIN_ASSOCIATION__OWNER;

	/**
	 * The feature ID for the '<em><b>Cascade Persist</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__CASCADE_PERSIST = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST;

	/**
	 * The feature ID for the '<em><b>Cascade Merge</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__CASCADE_MERGE = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE;

	/**
	 * The feature ID for the '<em><b>Cascade Remove</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__CASCADE_REMOVE = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE;

	/**
	 * The feature ID for the '<em><b>Cascade Refresh</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__CASCADE_REFRESH = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH;

	/**
	 * The feature ID for the '<em><b>Fetch Type Eager</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__FETCH_TYPE_EAGER = ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__DOMAIN_OBJECT = ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Target</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__TARGET = ABSTRACT_DOMAIN_ASSOCIATION__TARGET;

	/**
	 * The feature ID for the '<em><b>Tag</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__TAG = ABSTRACT_DOMAIN_ASSOCIATION__TAG;

	/**
	 * The feature ID for the '<em><b>Internal Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__INTERNAL_COMMENT = ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT;

	/**
	 * The feature ID for the '<em><b>User Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__USER_COMMENT = ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT;

	/**
	 * The feature ID for the '<em><b>Reverse Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__REVERSE_ASSOCIATION = ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION;

	/**
	 * The feature ID for the '<em><b>Insertable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__INSERTABLE = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Updatable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__UPDATABLE = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 1;

	/**
	 * The feature ID for the '<em><b>Optional</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__OPTIONAL = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 2;

	/**
	 * The feature ID for the '<em><b>Column</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION__COLUMN = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 3;

	/**
	 * The number of structural features of the '<em>Many To One Association</em>' class
	 * @generated
	 * @ordered
	 */
	int MANY_TO_ONE_ASSOCIATION_FEATURE_COUNT = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 4;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.OneToManyAssociationImpl <em>One To Many
	 * Association</em>}' class
	 * @see net.codecadenza.eclipse.model.domain.impl.OneToManyAssociationImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getOneToManyAssociation()
	 * @generated
	 */
	int ONE_TO_MANY_ASSOCIATION = 10;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__NAME = ABSTRACT_DOMAIN_ASSOCIATION__NAME;

	/**
	 * The feature ID for the '<em><b>Owner</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__OWNER = ABSTRACT_DOMAIN_ASSOCIATION__OWNER;

	/**
	 * The feature ID for the '<em><b>Cascade Persist</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__CASCADE_PERSIST = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST;

	/**
	 * The feature ID for the '<em><b>Cascade Merge</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__CASCADE_MERGE = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE;

	/**
	 * The feature ID for the '<em><b>Cascade Remove</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__CASCADE_REMOVE = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE;

	/**
	 * The feature ID for the '<em><b>Cascade Refresh</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__CASCADE_REFRESH = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH;

	/**
	 * The feature ID for the '<em><b>Fetch Type Eager</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__FETCH_TYPE_EAGER = ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__DOMAIN_OBJECT = ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Target</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__TARGET = ABSTRACT_DOMAIN_ASSOCIATION__TARGET;

	/**
	 * The feature ID for the '<em><b>Tag</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__TAG = ABSTRACT_DOMAIN_ASSOCIATION__TAG;

	/**
	 * The feature ID for the '<em><b>Internal Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__INTERNAL_COMMENT = ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT;

	/**
	 * The feature ID for the '<em><b>User Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__USER_COMMENT = ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT;

	/**
	 * The feature ID for the '<em><b>Reverse Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__REVERSE_ASSOCIATION = ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION;

	/**
	 * The feature ID for the '<em><b>Table</b></em>' containment reference
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION__TABLE = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>One To Many Association</em>' class
	 * @generated
	 * @ordered
	 */
	int ONE_TO_MANY_ASSOCIATION_FEATURE_COUNT = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 1;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.impl.OneToOneAssociationImpl <em>One To One
	 * Association</em>}' class
	 * @see net.codecadenza.eclipse.model.domain.impl.OneToOneAssociationImpl
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getOneToOneAssociation()
	 * @generated
	 */
	int ONE_TO_ONE_ASSOCIATION = 11;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__NAME = ABSTRACT_DOMAIN_ASSOCIATION__NAME;

	/**
	 * The feature ID for the '<em><b>Owner</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__OWNER = ABSTRACT_DOMAIN_ASSOCIATION__OWNER;

	/**
	 * The feature ID for the '<em><b>Cascade Persist</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__CASCADE_PERSIST = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST;

	/**
	 * The feature ID for the '<em><b>Cascade Merge</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__CASCADE_MERGE = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE;

	/**
	 * The feature ID for the '<em><b>Cascade Remove</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__CASCADE_REMOVE = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE;

	/**
	 * The feature ID for the '<em><b>Cascade Refresh</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__CASCADE_REFRESH = ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH;

	/**
	 * The feature ID for the '<em><b>Fetch Type Eager</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__FETCH_TYPE_EAGER = ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER;

	/**
	 * The feature ID for the '<em><b>Domain Object</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__DOMAIN_OBJECT = ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT;

	/**
	 * The feature ID for the '<em><b>Target</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__TARGET = ABSTRACT_DOMAIN_ASSOCIATION__TARGET;

	/**
	 * The feature ID for the '<em><b>Tag</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__TAG = ABSTRACT_DOMAIN_ASSOCIATION__TAG;

	/**
	 * The feature ID for the '<em><b>Internal Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__INTERNAL_COMMENT = ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT;

	/**
	 * The feature ID for the '<em><b>User Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__USER_COMMENT = ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT;

	/**
	 * The feature ID for the '<em><b>Reverse Association</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__REVERSE_ASSOCIATION = ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION;

	/**
	 * The feature ID for the '<em><b>Optional</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__OPTIONAL = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Column</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION__COLUMN = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>One To One Association</em>' class
	 * @generated
	 * @ordered
	 */
	int ONE_TO_ONE_ASSOCIATION_FEATURE_COUNT = ABSTRACT_DOMAIN_ASSOCIATION_FEATURE_COUNT + 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration <em>Discriminator
	 * Column Type Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDiscriminatorColumnTypeEnumeration()
	 * @generated
	 */
	int DISCRIMINATOR_COLUMN_TYPE_ENUMERATION = 12;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration <em>ID Generator Type
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getIDGeneratorTypeEnumeration()
	 * @generated
	 */
	int ID_GENERATOR_TYPE_ENUMERATION = 13;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration <em>Inheritance Type
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getInheritanceTypeEnumeration()
	 * @generated
	 */
	int INHERITANCE_TYPE_ENUMERATION = 14;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration <em>Temporal Type
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getTemporalTypeEnumeration()
	 * @generated
	 */
	int TEMPORAL_TYPE_ENUMERATION = 15;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.DomainTagEnumeration <em>Tag Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.domain.DomainTagEnumeration
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainTagEnumeration()
	 * @generated
	 */
	int DOMAIN_TAG_ENUMERATION = 16;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.AttributeTagEnumeration <em>Attribute Tag
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.domain.AttributeTagEnumeration
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getAttributeTagEnumeration()
	 * @generated
	 */
	int ATTRIBUTE_TAG_ENUMERATION = 17;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.AssociationTagEnumeration <em>Association Tag
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.domain.AssociationTagEnumeration
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getAssociationTagEnumeration()
	 * @generated
	 */
	int ASSOCIATION_TAG_ENUMERATION = 18;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration <em>Collection Type
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getCollectionTypeEnumeration()
	 * @generated
	 */
	int COLLECTION_TYPE_ENUMERATION = 19;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration <em>Collection
	 * Mapping Strategy Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration
	 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getCollectionMappingStrategyEnumeration()
	 * @generated
	 */
	int COLLECTION_MAPPING_STRATEGY_ENUMERATION = 20;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation <em>Abstract Domain
	 * Association</em>}'
	 * @return the meta object for class '<em>Abstract Domain Association</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation
	 * @generated
	 */
	EClass getAbstractDomainAssociation();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getName
	 * <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getName()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EAttribute getAbstractDomainAssociation_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isOwner
	 * <em>Owner</em>}'
	 * @return the meta object for the attribute '<em>Owner</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isOwner()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EAttribute getAbstractDomainAssociation_Owner();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadePersist <em>Cascade Persist</em>}'
	 * @return the meta object for the attribute '<em>Cascade Persist</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadePersist()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EAttribute getAbstractDomainAssociation_CascadePersist();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeMerge <em>Cascade Merge</em>}'
	 * @return the meta object for the attribute '<em>Cascade Merge</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeMerge()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EAttribute getAbstractDomainAssociation_CascadeMerge();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeRemove <em>Cascade Remove</em>}'
	 * @return the meta object for the attribute '<em>Cascade Remove</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeRemove()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EAttribute getAbstractDomainAssociation_CascadeRemove();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeRefresh <em>Cascade Refresh</em>}'
	 * @return the meta object for the attribute '<em>Cascade Refresh</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeRefresh()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EAttribute getAbstractDomainAssociation_CascadeRefresh();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isFetchTypeEager <em>Fetch Type Eager</em>}'
	 * @return the meta object for the attribute '<em>Fetch Type Eager</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isFetchTypeEager()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EAttribute getAbstractDomainAssociation_FetchTypeEager();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getDomainObject <em>Domain Object</em>}'
	 * @return the meta object for the container reference '<em>Domain Object</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getDomainObject()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EReference getAbstractDomainAssociation_DomainObject();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getTarget
	 * <em>Target</em>}'
	 * @return the meta object for the reference '<em>Target</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getTarget()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EReference getAbstractDomainAssociation_Target();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getTag
	 * <em>Tag</em>}'
	 * @return the meta object for the attribute '<em>Tag</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getTag()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EAttribute getAbstractDomainAssociation_Tag();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getInternalComment <em>Internal Comment</em>}'
	 * @return the meta object for the attribute '<em>Internal Comment</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getInternalComment()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EAttribute getAbstractDomainAssociation_InternalComment();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getUserComment <em>User Comment</em>}'
	 * @return the meta object for the attribute '<em>User Comment</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getUserComment()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EAttribute getAbstractDomainAssociation_UserComment();

	/**
	 * Return the meta object for the reference
	 * '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getReverseAssociation <em>Reverse Association</em>}'
	 * @return the meta object for the reference '<em>Reverse Association</em>'
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getReverseAssociation()
	 * @see #getAbstractDomainAssociation()
	 * @generated
	 */
	EReference getAbstractDomainAssociation_ReverseAssociation();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.DomainObject <em>Object</em>}'
	 * @return the meta object for class '<em>Object</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject
	 * @generated
	 */
	EClass getDomainObject();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainObject#getLabel <em>Label</em>}'
	 * @return the meta object for the attribute '<em>Label</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getLabel()
	 * @see #getDomainObject()
	 * @generated
	 */
	EAttribute getDomainObject_Label();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainObject#getNamePlural <em>Name
	 * Plural</em>}'
	 * @return the meta object for the attribute '<em>Name Plural</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getNamePlural()
	 * @see #getDomainObject()
	 * @generated
	 */
	EAttribute getDomainObject_NamePlural();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainObject#getLabelPlural <em>Label
	 * Plural</em>}'
	 * @return the meta object for the attribute '<em>Label Plural</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getLabelPlural()
	 * @see #getDomainObject()
	 * @generated
	 */
	EAttribute getDomainObject_LabelPlural();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorValue
	 * <em>Discriminator Value</em>}'
	 * @return the meta object for the attribute '<em>Discriminator Value</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorValue()
	 * @see #getDomainObject()
	 * @generated
	 */
	EAttribute getDomainObject_DiscriminatorValue();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorColumnType
	 * <em>Discriminator Column Type</em>}'
	 * @return the meta object for the attribute '<em>Discriminator Column Type</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorColumnType()
	 * @see #getDomainObject()
	 * @generated
	 */
	EAttribute getDomainObject_DiscriminatorColumnType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainObject#getInheritanceType
	 * <em>Inheritance Type</em>}'
	 * @return the meta object for the attribute '<em>Inheritance Type</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getInheritanceType()
	 * @see #getDomainObject()
	 * @generated
	 */
	EAttribute getDomainObject_InheritanceType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainObject#isPropertyAccess
	 * <em>Property Access</em>}'
	 * @return the meta object for the attribute '<em>Property Access</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#isPropertyAccess()
	 * @see #getDomainObject()
	 * @generated
	 */
	EAttribute getDomainObject_PropertyAccess();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainObject#isAbstract
	 * <em>Abstract</em>}'
	 * @return the meta object for the attribute '<em>Abstract</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#isAbstract()
	 * @see #getDomainObject()
	 * @generated
	 */
	EAttribute getDomainObject_Abstract();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainObject#isMappedSuperClass
	 * <em>Mapped Super Class</em>}'
	 * @return the meta object for the attribute '<em>Mapped Super Class</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#isMappedSuperClass()
	 * @see #getDomainObject()
	 * @generated
	 */
	EAttribute getDomainObject_MappedSuperClass();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.domain.DomainObject#getParent
	 * <em>Parent</em>}'
	 * @return the meta object for the reference '<em>Parent</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getParent()
	 * @see #getDomainObject()
	 * @generated
	 */
	EReference getDomainObject_Parent();

	/**
	 * Return the meta object for the containment reference '{@link net.codecadenza.eclipse.model.domain.DomainObject#getIDGenerator
	 * <em>ID Generator</em>}'
	 * @return the meta object for the containment reference '<em>ID Generator</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getIDGenerator()
	 * @see #getDomainObject()
	 * @generated
	 */
	EReference getDomainObject_IDGenerator();

	/**
	 * Return the meta object for the containment reference '{@link net.codecadenza.eclipse.model.domain.DomainObject#getInheritance
	 * <em>Inheritance</em>}'
	 * @return the meta object for the containment reference '<em>Inheritance</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getInheritance()
	 * @see #getDomainObject()
	 * @generated
	 */
	EReference getDomainObject_Inheritance();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.domain.DomainObject#getAttributes <em>Attributes</em>}'
	 * @return the meta object for the containment reference list '<em>Attributes</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAttributes()
	 * @see #getDomainObject()
	 * @generated
	 */
	EReference getDomainObject_Attributes();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.domain.DomainObject#getAssociations <em>Associations</em>}'
	 * @return the meta object for the containment reference list '<em>Associations</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAssociations()
	 * @see #getDomainObject()
	 * @generated
	 */
	EReference getDomainObject_Associations();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.domain.DomainObject#getEnumAssociations <em>Enum Associations</em>}'
	 * @return the meta object for the containment reference list '<em>Enum Associations</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getEnumAssociations()
	 * @see #getDomainObject()
	 * @generated
	 */
	EReference getDomainObject_EnumAssociations();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.domain.DomainObject#getTargetInheritances
	 * <em>Target Inheritances</em>}'
	 * @return the meta object for the reference list '<em>Target Inheritances</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getTargetInheritances()
	 * @see #getDomainObject()
	 * @generated
	 */
	EReference getDomainObject_TargetInheritances();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorColumn
	 * <em>Discriminator Column</em>}'
	 * @return the meta object for the reference '<em>Discriminator Column</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorColumn()
	 * @see #getDomainObject()
	 * @generated
	 */
	EReference getDomainObject_DiscriminatorColumn();

	/**
	 * Return the meta object for the containment reference
	 * '{@link net.codecadenza.eclipse.model.domain.DomainObject#getDatabaseTable <em>Database Table</em>}'
	 * @return the meta object for the containment reference '<em>Database Table</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getDatabaseTable()
	 * @see #getDomainObject()
	 * @generated
	 */
	EReference getDomainObject_DatabaseTable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainObject#getTag <em>Tag</em>}'
	 * @return the meta object for the attribute '<em>Tag</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getTag()
	 * @see #getDomainObject()
	 * @generated
	 */
	EAttribute getDomainObject_Tag();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.DomainAttribute <em>Domain Attribute</em>}'
	 * @return the meta object for class '<em>Domain Attribute</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute
	 * @generated
	 */
	EClass getDomainAttribute();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getName()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isPk <em>Pk</em>}'
	 * @return the meta object for the attribute '<em>Pk</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isPk()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_Pk();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getLabel
	 * <em>Label</em>}'
	 * @return the meta object for the attribute '<em>Label</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getLabel()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_Label();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getLabelPlural
	 * <em>Label Plural</em>}'
	 * @return the meta object for the attribute '<em>Label Plural</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getLabelPlural()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_LabelPlural();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isPersistent
	 * <em>Persistent</em>}'
	 * @return the meta object for the attribute '<em>Persistent</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isPersistent()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_Persistent();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isFetchTypeEager
	 * <em>Fetch Type Eager</em>}'
	 * @return the meta object for the attribute '<em>Fetch Type Eager</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isFetchTypeEager()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_FetchTypeEager();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isInsertable
	 * <em>Insertable</em>}'
	 * @return the meta object for the attribute '<em>Insertable</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isInsertable()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_Insertable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isUpdatable
	 * <em>Updatable</em>}'
	 * @return the meta object for the attribute '<em>Updatable</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isUpdatable()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_Updatable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isTrackVersion
	 * <em>Track Version</em>}'
	 * @return the meta object for the attribute '<em>Track Version</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isTrackVersion()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_TrackVersion();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isSetDateOnPersist
	 * <em>Set Date On Persist</em>}'
	 * @return the meta object for the attribute '<em>Set Date On Persist</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isSetDateOnPersist()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_SetDateOnPersist();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isSetDateOnUpdate
	 * <em>Set Date On Update</em>}'
	 * @return the meta object for the attribute '<em>Set Date On Update</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isSetDateOnUpdate()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_SetDateOnUpdate();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isDisplayAttribute
	 * <em>Display Attribute</em>}'
	 * @return the meta object for the attribute '<em>Display Attribute</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isDisplayAttribute()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_DisplayAttribute();

	/**
	 * Return the meta object for the container reference
	 * '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainObject <em>Domain Object</em>}'
	 * @return the meta object for the container reference '<em>Domain Object</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainObject()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EReference getDomainAttribute_DomainObject();

	/**
	 * Return the meta object for the containment reference
	 * '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainAttributeValidator <em>Domain Attribute
	 * Validator</em>}'
	 * @return the meta object for the containment reference '<em>Domain Attribute Validator</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainAttributeValidator()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EReference getDomainAttribute_DomainAttributeValidator();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getTemporalType
	 * <em>Temporal Type</em>}'
	 * @return the meta object for the attribute '<em>Temporal Type</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getTemporalType()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_TemporalType();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getJavaType <em>Java
	 * Type</em>}'
	 * @return the meta object for the reference '<em>Java Type</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getJavaType()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EReference getDomainAttribute_JavaType();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getColumn
	 * <em>Column</em>}'
	 * @return the meta object for the reference '<em>Column</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getColumn()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EReference getDomainAttribute_Column();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getTag <em>Tag</em>}'
	 * @return the meta object for the attribute '<em>Tag</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getTag()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_Tag();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isLob <em>Lob</em>}'
	 * @return the meta object for the attribute '<em>Lob</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isLob()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_Lob();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getInternalComment
	 * <em>Internal Comment</em>}'
	 * @return the meta object for the attribute '<em>Internal Comment</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getInternalComment()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_InternalComment();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getUserComment <em>User
	 * Comment</em>}'
	 * @return the meta object for the attribute '<em>User Comment</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getUserComment()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_UserComment();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isRemoveWhitespaceCharacters <em>Remove Whitespace
	 * Characters</em>}'
	 * @return the meta object for the attribute '<em>Remove Whitespace Characters</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isRemoveWhitespaceCharacters()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_RemoveWhitespaceCharacters();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isConvertToUpperCase
	 * <em>Convert To Upper Case</em>}'
	 * @return the meta object for the attribute '<em>Convert To Upper Case</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isConvertToUpperCase()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_ConvertToUpperCase();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isConvertToLowerCase
	 * <em>Convert To Lower Case</em>}'
	 * @return the meta object for the attribute '<em>Convert To Lower Case</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#isConvertToLowerCase()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_ConvertToLowerCase();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionType
	 * <em>Collection Type</em>}'
	 * @return the meta object for the attribute '<em>Collection Type</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionType()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_CollectionType();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionMappingStrategy <em>Collection Mapping
	 * Strategy</em>}'
	 * @return the meta object for the attribute '<em>Collection Mapping Strategy</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionMappingStrategy()
	 * @see #getDomainAttribute()
	 * @generated
	 */
	EAttribute getDomainAttribute_CollectionMappingStrategy();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator <em>Attribute
	 * Validator</em>}'
	 * @return the meta object for class '<em>Attribute Validator</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator
	 * @generated
	 */
	EClass getDomainAttributeValidator();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isFutureDate
	 * <em>Future Date</em>}'
	 * @return the meta object for the attribute '<em>Future Date</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isFutureDate()
	 * @see #getDomainAttributeValidator()
	 * @generated
	 */
	EAttribute getDomainAttributeValidator_FutureDate();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isPastDate
	 * <em>Past Date</em>}'
	 * @return the meta object for the attribute '<em>Past Date</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isPastDate()
	 * @see #getDomainAttributeValidator()
	 * @generated
	 */
	EAttribute getDomainAttributeValidator_PastDate();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMaxLength
	 * <em>Max Length</em>}'
	 * @return the meta object for the attribute '<em>Max Length</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMaxLength()
	 * @see #getDomainAttributeValidator()
	 * @generated
	 */
	EAttribute getDomainAttributeValidator_MaxLength();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMinLength
	 * <em>Min Length</em>}'
	 * @return the meta object for the attribute '<em>Min Length</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMinLength()
	 * @see #getDomainAttributeValidator()
	 * @generated
	 */
	EAttribute getDomainAttributeValidator_MinLength();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isNullable
	 * <em>Nullable</em>}'
	 * @return the meta object for the attribute '<em>Nullable</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#isNullable()
	 * @see #getDomainAttributeValidator()
	 * @generated
	 */
	EAttribute getDomainAttributeValidator_Nullable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMaxValue
	 * <em>Max Value</em>}'
	 * @return the meta object for the attribute '<em>Max Value</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMaxValue()
	 * @see #getDomainAttributeValidator()
	 * @generated
	 */
	EAttribute getDomainAttributeValidator_MaxValue();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMinValue
	 * <em>Min Value</em>}'
	 * @return the meta object for the attribute '<em>Min Value</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getMinValue()
	 * @see #getDomainAttributeValidator()
	 * @generated
	 */
	EAttribute getDomainAttributeValidator_MinValue();

	/**
	 * Return the meta object for the attribute
	 * '{@link net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getRegularExpression <em>Regular Expression</em>}'
	 * @return the meta object for the attribute '<em>Regular Expression</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainAttributeValidator#getRegularExpression()
	 * @see #getDomainAttributeValidator()
	 * @generated
	 */
	EAttribute getDomainAttributeValidator_RegularExpression();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.DomainInheritance <em>Inheritance</em>}'
	 * @return the meta object for class '<em>Inheritance</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainInheritance
	 * @generated
	 */
	EClass getDomainInheritance();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.domain.DomainInheritance#getSource
	 * <em>Source</em>}'
	 * @return the meta object for the container reference '<em>Source</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainInheritance#getSource()
	 * @see #getDomainInheritance()
	 * @generated
	 */
	EReference getDomainInheritance_Source();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.domain.DomainInheritance#getTarget
	 * <em>Target</em>}'
	 * @return the meta object for the reference '<em>Target</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainInheritance#getTarget()
	 * @see #getDomainInheritance()
	 * @generated
	 */
	EReference getDomainInheritance_Target();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.DomainNamespace <em>Namespace</em>}'
	 * @return the meta object for class '<em>Namespace</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainNamespace
	 * @generated
	 */
	EClass getDomainNamespace();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.domain.DomainNamespace#getDomainObjects <em>Domain Objects</em>}'
	 * @return the meta object for the containment reference list '<em>Domain Objects</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainNamespace#getDomainObjects()
	 * @see #getDomainNamespace()
	 * @generated
	 */
	EReference getDomainNamespace_DomainObjects();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.domain.DomainNamespace#getEnumerations <em>Enumerations</em>}'
	 * @return the meta object for the containment reference list '<em>Enumerations</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainNamespace#getEnumerations()
	 * @see #getDomainNamespace()
	 * @generated
	 */
	EReference getDomainNamespace_Enumerations();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.EnumAssociation <em>Enum Association</em>}'
	 * @return the meta object for class '<em>Enum Association</em>'
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation
	 * @generated
	 */
	EClass getEnumAssociation();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.domain.EnumAssociation#getSource
	 * <em>Source</em>}'
	 * @return the meta object for the container reference '<em>Source</em>'
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation#getSource()
	 * @see #getEnumAssociation()
	 * @generated
	 */
	EReference getEnumAssociation_Source();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.domain.EnumAssociation#getTarget
	 * <em>Target</em>}'
	 * @return the meta object for the reference '<em>Target</em>'
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation#getTarget()
	 * @see #getEnumAssociation()
	 * @generated
	 */
	EReference getEnumAssociation_Target();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.domain.EnumAssociation#getDomainAttribute
	 * <em>Domain Attribute</em>}'
	 * @return the meta object for the reference '<em>Domain Attribute</em>'
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation#getDomainAttribute()
	 * @see #getEnumAssociation()
	 * @generated
	 */
	EReference getEnumAssociation_DomainAttribute();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.IDGenerator <em>ID Generator</em>}'
	 * @return the meta object for class '<em>ID Generator</em>'
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator
	 * @generated
	 */
	EClass getIDGenerator();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.IDGenerator#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#getName()
	 * @see #getIDGenerator()
	 * @generated
	 */
	EAttribute getIDGenerator_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.IDGenerator#getBlockSize <em>Block
	 * Size</em>}'
	 * @return the meta object for the attribute '<em>Block Size</em>'
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#getBlockSize()
	 * @see #getIDGenerator()
	 * @generated
	 */
	EAttribute getIDGenerator_BlockSize();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.IDGenerator#getInitialValue <em>Initial
	 * Value</em>}'
	 * @return the meta object for the attribute '<em>Initial Value</em>'
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#getInitialValue()
	 * @see #getIDGenerator()
	 * @generated
	 */
	EAttribute getIDGenerator_InitialValue();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.IDGenerator#getGeneratorType
	 * <em>Generator Type</em>}'
	 * @return the meta object for the attribute '<em>Generator Type</em>'
	 * @see net.codecadenza.eclipse.model.domain.IDGenerator#getGeneratorType()
	 * @see #getIDGenerator()
	 * @generated
	 */
	EAttribute getIDGenerator_GeneratorType();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.ManyToManyAssociation <em>Many To Many
	 * Association</em>}'
	 * @return the meta object for class '<em>Many To Many Association</em>'
	 * @see net.codecadenza.eclipse.model.domain.ManyToManyAssociation
	 * @generated
	 */
	EClass getManyToManyAssociation();

	/**
	 * Return the meta object for the containment reference
	 * '{@link net.codecadenza.eclipse.model.domain.ManyToManyAssociation#getTable <em>Table</em>}'
	 * @return the meta object for the containment reference '<em>Table</em>'
	 * @see net.codecadenza.eclipse.model.domain.ManyToManyAssociation#getTable()
	 * @see #getManyToManyAssociation()
	 * @generated
	 */
	EReference getManyToManyAssociation_Table();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation <em>Many To One
	 * Association</em>}'
	 * @return the meta object for class '<em>Many To One Association</em>'
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation
	 * @generated
	 */
	EClass getManyToOneAssociation();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isInsertable
	 * <em>Insertable</em>}'
	 * @return the meta object for the attribute '<em>Insertable</em>'
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isInsertable()
	 * @see #getManyToOneAssociation()
	 * @generated
	 */
	EAttribute getManyToOneAssociation_Insertable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isUpdatable
	 * <em>Updatable</em>}'
	 * @return the meta object for the attribute '<em>Updatable</em>'
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isUpdatable()
	 * @see #getManyToOneAssociation()
	 * @generated
	 */
	EAttribute getManyToOneAssociation_Updatable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isOptional
	 * <em>Optional</em>}'
	 * @return the meta object for the attribute '<em>Optional</em>'
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#isOptional()
	 * @see #getManyToOneAssociation()
	 * @generated
	 */
	EAttribute getManyToOneAssociation_Optional();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.domain.ManyToOneAssociation#getColumn
	 * <em>Column</em>}'
	 * @return the meta object for the reference '<em>Column</em>'
	 * @see net.codecadenza.eclipse.model.domain.ManyToOneAssociation#getColumn()
	 * @see #getManyToOneAssociation()
	 * @generated
	 */
	EReference getManyToOneAssociation_Column();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.OneToManyAssociation <em>One To Many
	 * Association</em>}'
	 * @return the meta object for class '<em>One To Many Association</em>'
	 * @see net.codecadenza.eclipse.model.domain.OneToManyAssociation
	 * @generated
	 */
	EClass getOneToManyAssociation();

	/**
	 * Return the meta object for the containment reference
	 * '{@link net.codecadenza.eclipse.model.domain.OneToManyAssociation#getTable <em>Table</em>}'
	 * @return the meta object for the containment reference '<em>Table</em>'
	 * @see net.codecadenza.eclipse.model.domain.OneToManyAssociation#getTable()
	 * @see #getOneToManyAssociation()
	 * @generated
	 */
	EReference getOneToManyAssociation_Table();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.domain.OneToOneAssociation <em>One To One
	 * Association</em>}'
	 * @return the meta object for class '<em>One To One Association</em>'
	 * @see net.codecadenza.eclipse.model.domain.OneToOneAssociation
	 * @generated
	 */
	EClass getOneToOneAssociation();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.domain.OneToOneAssociation#isOptional
	 * <em>Optional</em>}'
	 * @return the meta object for the attribute '<em>Optional</em>'
	 * @see net.codecadenza.eclipse.model.domain.OneToOneAssociation#isOptional()
	 * @see #getOneToOneAssociation()
	 * @generated
	 */
	EAttribute getOneToOneAssociation_Optional();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.domain.OneToOneAssociation#getColumn
	 * <em>Column</em>}'
	 * @return the meta object for the reference '<em>Column</em>'
	 * @see net.codecadenza.eclipse.model.domain.OneToOneAssociation#getColumn()
	 * @see #getOneToOneAssociation()
	 * @generated
	 */
	EReference getOneToOneAssociation_Column();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration
	 * <em>Discriminator Column Type Enumeration</em>}'
	 * @return the meta object for enum '<em>Discriminator Column Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration
	 * @generated
	 */
	EEnum getDiscriminatorColumnTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration <em>ID Generator Type
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>ID Generator Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration
	 * @generated
	 */
	EEnum getIDGeneratorTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration <em>Inheritance Type
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Inheritance Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration
	 * @generated
	 */
	EEnum getInheritanceTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration <em>Temporal Type
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Temporal Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration
	 * @generated
	 */
	EEnum getTemporalTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.domain.DomainTagEnumeration <em>Tag Enumeration</em>}'
	 * @return the meta object for enum '<em>Tag Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.domain.DomainTagEnumeration
	 * @generated
	 */
	EEnum getDomainTagEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.domain.AttributeTagEnumeration <em>Attribute Tag
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Attribute Tag Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.domain.AttributeTagEnumeration
	 * @generated
	 */
	EEnum getAttributeTagEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.domain.AssociationTagEnumeration <em>Association Tag
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Association Tag Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.domain.AssociationTagEnumeration
	 * @generated
	 */
	EEnum getAssociationTagEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration <em>Collection Type
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Collection Type Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration
	 * @generated
	 */
	EEnum getCollectionTypeEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration
	 * <em>Collection Mapping Strategy Enumeration</em>}'
	 * @return the meta object for enum '<em>Collection Mapping Strategy Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration
	 * @generated
	 */
	EEnum getCollectionMappingStrategyEnumeration();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	DomainFactory getDomainFactory();

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
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl
		 * <em>Abstract Domain Association</em>}' class
		 * @see net.codecadenza.eclipse.model.domain.impl.AbstractDomainAssociationImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getAbstractDomainAssociation()
		 * @generated
		 */
		EClass ABSTRACT_DOMAIN_ASSOCIATION = eINSTANCE.getAbstractDomainAssociation();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_DOMAIN_ASSOCIATION__NAME = eINSTANCE.getAbstractDomainAssociation_Name();

		/**
		 * The meta object literal for the '<em><b>Owner</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_DOMAIN_ASSOCIATION__OWNER = eINSTANCE.getAbstractDomainAssociation_Owner();

		/**
		 * The meta object literal for the '<em><b>Cascade Persist</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_PERSIST = eINSTANCE.getAbstractDomainAssociation_CascadePersist();

		/**
		 * The meta object literal for the '<em><b>Cascade Merge</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_MERGE = eINSTANCE.getAbstractDomainAssociation_CascadeMerge();

		/**
		 * The meta object literal for the '<em><b>Cascade Remove</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REMOVE = eINSTANCE.getAbstractDomainAssociation_CascadeRemove();

		/**
		 * The meta object literal for the '<em><b>Cascade Refresh</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_DOMAIN_ASSOCIATION__CASCADE_REFRESH = eINSTANCE.getAbstractDomainAssociation_CascadeRefresh();

		/**
		 * The meta object literal for the '<em><b>Fetch Type Eager</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_DOMAIN_ASSOCIATION__FETCH_TYPE_EAGER = eINSTANCE.getAbstractDomainAssociation_FetchTypeEager();

		/**
		 * The meta object literal for the '<em><b>Domain Object</b></em>' container reference feature
		 * @generated
		 */
		EReference ABSTRACT_DOMAIN_ASSOCIATION__DOMAIN_OBJECT = eINSTANCE.getAbstractDomainAssociation_DomainObject();

		/**
		 * The meta object literal for the '<em><b>Target</b></em>' reference feature
		 * @generated
		 */
		EReference ABSTRACT_DOMAIN_ASSOCIATION__TARGET = eINSTANCE.getAbstractDomainAssociation_Target();

		/**
		 * The meta object literal for the '<em><b>Tag</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_DOMAIN_ASSOCIATION__TAG = eINSTANCE.getAbstractDomainAssociation_Tag();

		/**
		 * The meta object literal for the '<em><b>Internal Comment</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_DOMAIN_ASSOCIATION__INTERNAL_COMMENT = eINSTANCE.getAbstractDomainAssociation_InternalComment();

		/**
		 * The meta object literal for the '<em><b>User Comment</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ABSTRACT_DOMAIN_ASSOCIATION__USER_COMMENT = eINSTANCE.getAbstractDomainAssociation_UserComment();

		/**
		 * The meta object literal for the '<em><b>Reverse Association</b></em>' reference feature
		 * @generated
		 */
		EReference ABSTRACT_DOMAIN_ASSOCIATION__REVERSE_ASSOCIATION = eINSTANCE.getAbstractDomainAssociation_ReverseAssociation();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl <em>Object</em>}' class
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainObjectImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainObject()
		 * @generated
		 */
		EClass DOMAIN_OBJECT = eINSTANCE.getDomainObject();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_OBJECT__LABEL = eINSTANCE.getDomainObject_Label();

		/**
		 * The meta object literal for the '<em><b>Name Plural</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_OBJECT__NAME_PLURAL = eINSTANCE.getDomainObject_NamePlural();

		/**
		 * The meta object literal for the '<em><b>Label Plural</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_OBJECT__LABEL_PLURAL = eINSTANCE.getDomainObject_LabelPlural();

		/**
		 * The meta object literal for the '<em><b>Discriminator Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_OBJECT__DISCRIMINATOR_VALUE = eINSTANCE.getDomainObject_DiscriminatorValue();

		/**
		 * The meta object literal for the '<em><b>Discriminator Column Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_OBJECT__DISCRIMINATOR_COLUMN_TYPE = eINSTANCE.getDomainObject_DiscriminatorColumnType();

		/**
		 * The meta object literal for the '<em><b>Inheritance Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_OBJECT__INHERITANCE_TYPE = eINSTANCE.getDomainObject_InheritanceType();

		/**
		 * The meta object literal for the '<em><b>Property Access</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_OBJECT__PROPERTY_ACCESS = eINSTANCE.getDomainObject_PropertyAccess();

		/**
		 * The meta object literal for the '<em><b>Abstract</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_OBJECT__ABSTRACT = eINSTANCE.getDomainObject_Abstract();

		/**
		 * The meta object literal for the '<em><b>Mapped Super Class</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_OBJECT__MAPPED_SUPER_CLASS = eINSTANCE.getDomainObject_MappedSuperClass();

		/**
		 * The meta object literal for the '<em><b>Parent</b></em>' reference feature
		 * @generated
		 */
		EReference DOMAIN_OBJECT__PARENT = eINSTANCE.getDomainObject_Parent();

		/**
		 * The meta object literal for the '<em><b>ID Generator</b></em>' containment reference feature
		 * @generated
		 */
		EReference DOMAIN_OBJECT__ID_GENERATOR = eINSTANCE.getDomainObject_IDGenerator();

		/**
		 * The meta object literal for the '<em><b>Inheritance</b></em>' containment reference feature
		 * @generated
		 */
		EReference DOMAIN_OBJECT__INHERITANCE = eINSTANCE.getDomainObject_Inheritance();

		/**
		 * The meta object literal for the '<em><b>Attributes</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DOMAIN_OBJECT__ATTRIBUTES = eINSTANCE.getDomainObject_Attributes();

		/**
		 * The meta object literal for the '<em><b>Associations</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DOMAIN_OBJECT__ASSOCIATIONS = eINSTANCE.getDomainObject_Associations();

		/**
		 * The meta object literal for the '<em><b>Enum Associations</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DOMAIN_OBJECT__ENUM_ASSOCIATIONS = eINSTANCE.getDomainObject_EnumAssociations();

		/**
		 * The meta object literal for the '<em><b>Target Inheritances</b></em>' reference list feature
		 * @generated
		 */
		EReference DOMAIN_OBJECT__TARGET_INHERITANCES = eINSTANCE.getDomainObject_TargetInheritances();

		/**
		 * The meta object literal for the '<em><b>Discriminator Column</b></em>' reference feature
		 * @generated
		 */
		EReference DOMAIN_OBJECT__DISCRIMINATOR_COLUMN = eINSTANCE.getDomainObject_DiscriminatorColumn();

		/**
		 * The meta object literal for the '<em><b>Database Table</b></em>' containment reference feature
		 * @generated
		 */
		EReference DOMAIN_OBJECT__DATABASE_TABLE = eINSTANCE.getDomainObject_DatabaseTable();

		/**
		 * The meta object literal for the '<em><b>Tag</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_OBJECT__TAG = eINSTANCE.getDomainObject_Tag();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl <em>Domain
		 * Attribute</em>}' class
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainAttributeImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainAttribute()
		 * @generated
		 */
		EClass DOMAIN_ATTRIBUTE = eINSTANCE.getDomainAttribute();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__NAME = eINSTANCE.getDomainAttribute_Name();

		/**
		 * The meta object literal for the '<em><b>Pk</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__PK = eINSTANCE.getDomainAttribute_Pk();

		/**
		 * The meta object literal for the '<em><b>Label</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__LABEL = eINSTANCE.getDomainAttribute_Label();

		/**
		 * The meta object literal for the '<em><b>Label Plural</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__LABEL_PLURAL = eINSTANCE.getDomainAttribute_LabelPlural();

		/**
		 * The meta object literal for the '<em><b>Persistent</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__PERSISTENT = eINSTANCE.getDomainAttribute_Persistent();

		/**
		 * The meta object literal for the '<em><b>Fetch Type Eager</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__FETCH_TYPE_EAGER = eINSTANCE.getDomainAttribute_FetchTypeEager();

		/**
		 * The meta object literal for the '<em><b>Insertable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__INSERTABLE = eINSTANCE.getDomainAttribute_Insertable();

		/**
		 * The meta object literal for the '<em><b>Updatable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__UPDATABLE = eINSTANCE.getDomainAttribute_Updatable();

		/**
		 * The meta object literal for the '<em><b>Track Version</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__TRACK_VERSION = eINSTANCE.getDomainAttribute_TrackVersion();

		/**
		 * The meta object literal for the '<em><b>Set Date On Persist</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__SET_DATE_ON_PERSIST = eINSTANCE.getDomainAttribute_SetDateOnPersist();

		/**
		 * The meta object literal for the '<em><b>Set Date On Update</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__SET_DATE_ON_UPDATE = eINSTANCE.getDomainAttribute_SetDateOnUpdate();

		/**
		 * The meta object literal for the '<em><b>Display Attribute</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__DISPLAY_ATTRIBUTE = eINSTANCE.getDomainAttribute_DisplayAttribute();

		/**
		 * The meta object literal for the '<em><b>Domain Object</b></em>' container reference feature
		 * @generated
		 */
		EReference DOMAIN_ATTRIBUTE__DOMAIN_OBJECT = eINSTANCE.getDomainAttribute_DomainObject();

		/**
		 * The meta object literal for the '<em><b>Domain Attribute Validator</b></em>' containment reference feature
		 * @generated
		 */
		EReference DOMAIN_ATTRIBUTE__DOMAIN_ATTRIBUTE_VALIDATOR = eINSTANCE.getDomainAttribute_DomainAttributeValidator();

		/**
		 * The meta object literal for the '<em><b>Temporal Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__TEMPORAL_TYPE = eINSTANCE.getDomainAttribute_TemporalType();

		/**
		 * The meta object literal for the '<em><b>Java Type</b></em>' reference feature
		 * @generated
		 */
		EReference DOMAIN_ATTRIBUTE__JAVA_TYPE = eINSTANCE.getDomainAttribute_JavaType();

		/**
		 * The meta object literal for the '<em><b>Column</b></em>' reference feature
		 * @generated
		 */
		EReference DOMAIN_ATTRIBUTE__COLUMN = eINSTANCE.getDomainAttribute_Column();

		/**
		 * The meta object literal for the '<em><b>Tag</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__TAG = eINSTANCE.getDomainAttribute_Tag();

		/**
		 * The meta object literal for the '<em><b>Lob</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__LOB = eINSTANCE.getDomainAttribute_Lob();

		/**
		 * The meta object literal for the '<em><b>Internal Comment</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__INTERNAL_COMMENT = eINSTANCE.getDomainAttribute_InternalComment();

		/**
		 * The meta object literal for the '<em><b>User Comment</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__USER_COMMENT = eINSTANCE.getDomainAttribute_UserComment();

		/**
		 * The meta object literal for the '<em><b>Remove Whitespace Characters</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__REMOVE_WHITESPACE_CHARACTERS = eINSTANCE.getDomainAttribute_RemoveWhitespaceCharacters();

		/**
		 * The meta object literal for the '<em><b>Convert To Upper Case</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__CONVERT_TO_UPPER_CASE = eINSTANCE.getDomainAttribute_ConvertToUpperCase();

		/**
		 * The meta object literal for the '<em><b>Convert To Lower Case</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__CONVERT_TO_LOWER_CASE = eINSTANCE.getDomainAttribute_ConvertToLowerCase();

		/**
		 * The meta object literal for the '<em><b>Collection Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__COLLECTION_TYPE = eINSTANCE.getDomainAttribute_CollectionType();

		/**
		 * The meta object literal for the '<em><b>Collection Mapping Strategy</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE__COLLECTION_MAPPING_STRATEGY = eINSTANCE.getDomainAttribute_CollectionMappingStrategy();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl
		 * <em>Attribute Validator</em>}' class
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainAttributeValidatorImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainAttributeValidator()
		 * @generated
		 */
		EClass DOMAIN_ATTRIBUTE_VALIDATOR = eINSTANCE.getDomainAttributeValidator();

		/**
		 * The meta object literal for the '<em><b>Future Date</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE_VALIDATOR__FUTURE_DATE = eINSTANCE.getDomainAttributeValidator_FutureDate();

		/**
		 * The meta object literal for the '<em><b>Past Date</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE_VALIDATOR__PAST_DATE = eINSTANCE.getDomainAttributeValidator_PastDate();

		/**
		 * The meta object literal for the '<em><b>Max Length</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE_VALIDATOR__MAX_LENGTH = eINSTANCE.getDomainAttributeValidator_MaxLength();

		/**
		 * The meta object literal for the '<em><b>Min Length</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE_VALIDATOR__MIN_LENGTH = eINSTANCE.getDomainAttributeValidator_MinLength();

		/**
		 * The meta object literal for the '<em><b>Nullable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE_VALIDATOR__NULLABLE = eINSTANCE.getDomainAttributeValidator_Nullable();

		/**
		 * The meta object literal for the '<em><b>Max Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE_VALIDATOR__MAX_VALUE = eINSTANCE.getDomainAttributeValidator_MaxValue();

		/**
		 * The meta object literal for the '<em><b>Min Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE_VALIDATOR__MIN_VALUE = eINSTANCE.getDomainAttributeValidator_MinValue();

		/**
		 * The meta object literal for the '<em><b>Regular Expression</b></em>' attribute feature
		 * @generated
		 */
		EAttribute DOMAIN_ATTRIBUTE_VALIDATOR__REGULAR_EXPRESSION = eINSTANCE.getDomainAttributeValidator_RegularExpression();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.DomainInheritanceImpl
		 * <em>Inheritance</em>}' class
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainInheritanceImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainInheritance()
		 * @generated
		 */
		EClass DOMAIN_INHERITANCE = eINSTANCE.getDomainInheritance();

		/**
		 * The meta object literal for the '<em><b>Source</b></em>' container reference feature
		 * @generated
		 */
		EReference DOMAIN_INHERITANCE__SOURCE = eINSTANCE.getDomainInheritance_Source();

		/**
		 * The meta object literal for the '<em><b>Target</b></em>' reference feature
		 * @generated
		 */
		EReference DOMAIN_INHERITANCE__TARGET = eINSTANCE.getDomainInheritance_Target();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.DomainNamespaceImpl <em>Namespace</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainNamespaceImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainNamespace()
		 * @generated
		 */
		EClass DOMAIN_NAMESPACE = eINSTANCE.getDomainNamespace();

		/**
		 * The meta object literal for the '<em><b>Domain Objects</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DOMAIN_NAMESPACE__DOMAIN_OBJECTS = eINSTANCE.getDomainNamespace_DomainObjects();

		/**
		 * The meta object literal for the '<em><b>Enumerations</b></em>' containment reference list feature
		 * @generated
		 */
		EReference DOMAIN_NAMESPACE__ENUMERATIONS = eINSTANCE.getDomainNamespace_Enumerations();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.EnumAssociationImpl <em>Enum
		 * Association</em>}' class
		 * @see net.codecadenza.eclipse.model.domain.impl.EnumAssociationImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getEnumAssociation()
		 * @generated
		 */
		EClass ENUM_ASSOCIATION = eINSTANCE.getEnumAssociation();

		/**
		 * The meta object literal for the '<em><b>Source</b></em>' container reference feature
		 * @generated
		 */
		EReference ENUM_ASSOCIATION__SOURCE = eINSTANCE.getEnumAssociation_Source();

		/**
		 * The meta object literal for the '<em><b>Target</b></em>' reference feature
		 * @generated
		 */
		EReference ENUM_ASSOCIATION__TARGET = eINSTANCE.getEnumAssociation_Target();

		/**
		 * The meta object literal for the '<em><b>Domain Attribute</b></em>' reference feature
		 * @generated
		 */
		EReference ENUM_ASSOCIATION__DOMAIN_ATTRIBUTE = eINSTANCE.getEnumAssociation_DomainAttribute();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.IDGeneratorImpl <em>ID Generator</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.domain.impl.IDGeneratorImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getIDGenerator()
		 * @generated
		 */
		EClass ID_GENERATOR = eINSTANCE.getIDGenerator();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ID_GENERATOR__NAME = eINSTANCE.getIDGenerator_Name();

		/**
		 * The meta object literal for the '<em><b>Block Size</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ID_GENERATOR__BLOCK_SIZE = eINSTANCE.getIDGenerator_BlockSize();

		/**
		 * The meta object literal for the '<em><b>Initial Value</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ID_GENERATOR__INITIAL_VALUE = eINSTANCE.getIDGenerator_InitialValue();

		/**
		 * The meta object literal for the '<em><b>Generator Type</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ID_GENERATOR__GENERATOR_TYPE = eINSTANCE.getIDGenerator_GeneratorType();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.ManyToManyAssociationImpl <em>Many To
		 * Many Association</em>}' class
		 * @see net.codecadenza.eclipse.model.domain.impl.ManyToManyAssociationImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getManyToManyAssociation()
		 * @generated
		 */
		EClass MANY_TO_MANY_ASSOCIATION = eINSTANCE.getManyToManyAssociation();

		/**
		 * The meta object literal for the '<em><b>Table</b></em>' containment reference feature
		 * @generated
		 */
		EReference MANY_TO_MANY_ASSOCIATION__TABLE = eINSTANCE.getManyToManyAssociation_Table();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.ManyToOneAssociationImpl <em>Many To One
		 * Association</em>}' class
		 * @see net.codecadenza.eclipse.model.domain.impl.ManyToOneAssociationImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getManyToOneAssociation()
		 * @generated
		 */
		EClass MANY_TO_ONE_ASSOCIATION = eINSTANCE.getManyToOneAssociation();

		/**
		 * The meta object literal for the '<em><b>Insertable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute MANY_TO_ONE_ASSOCIATION__INSERTABLE = eINSTANCE.getManyToOneAssociation_Insertable();

		/**
		 * The meta object literal for the '<em><b>Updatable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute MANY_TO_ONE_ASSOCIATION__UPDATABLE = eINSTANCE.getManyToOneAssociation_Updatable();

		/**
		 * The meta object literal for the '<em><b>Optional</b></em>' attribute feature
		 * @generated
		 */
		EAttribute MANY_TO_ONE_ASSOCIATION__OPTIONAL = eINSTANCE.getManyToOneAssociation_Optional();

		/**
		 * The meta object literal for the '<em><b>Column</b></em>' reference feature
		 * @generated
		 */
		EReference MANY_TO_ONE_ASSOCIATION__COLUMN = eINSTANCE.getManyToOneAssociation_Column();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.OneToManyAssociationImpl <em>One To Many
		 * Association</em>}' class
		 * @see net.codecadenza.eclipse.model.domain.impl.OneToManyAssociationImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getOneToManyAssociation()
		 * @generated
		 */
		EClass ONE_TO_MANY_ASSOCIATION = eINSTANCE.getOneToManyAssociation();

		/**
		 * The meta object literal for the '<em><b>Table</b></em>' containment reference feature
		 * @generated
		 */
		EReference ONE_TO_MANY_ASSOCIATION__TABLE = eINSTANCE.getOneToManyAssociation_Table();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.impl.OneToOneAssociationImpl <em>One To One
		 * Association</em>}' class
		 * @see net.codecadenza.eclipse.model.domain.impl.OneToOneAssociationImpl
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getOneToOneAssociation()
		 * @generated
		 */
		EClass ONE_TO_ONE_ASSOCIATION = eINSTANCE.getOneToOneAssociation();

		/**
		 * The meta object literal for the '<em><b>Optional</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ONE_TO_ONE_ASSOCIATION__OPTIONAL = eINSTANCE.getOneToOneAssociation_Optional();

		/**
		 * The meta object literal for the '<em><b>Column</b></em>' reference feature
		 * @generated
		 */
		EReference ONE_TO_ONE_ASSOCIATION__COLUMN = eINSTANCE.getOneToOneAssociation_Column();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration
		 * <em>Discriminator Column Type Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDiscriminatorColumnTypeEnumeration()
		 * @generated
		 */
		EEnum DISCRIMINATOR_COLUMN_TYPE_ENUMERATION = eINSTANCE.getDiscriminatorColumnTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration <em>ID Generator
		 * Type Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getIDGeneratorTypeEnumeration()
		 * @generated
		 */
		EEnum ID_GENERATOR_TYPE_ENUMERATION = eINSTANCE.getIDGeneratorTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration <em>Inheritance
		 * Type Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getInheritanceTypeEnumeration()
		 * @generated
		 */
		EEnum INHERITANCE_TYPE_ENUMERATION = eINSTANCE.getInheritanceTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration <em>Temporal Type
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getTemporalTypeEnumeration()
		 * @generated
		 */
		EEnum TEMPORAL_TYPE_ENUMERATION = eINSTANCE.getTemporalTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.DomainTagEnumeration <em>Tag
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.domain.DomainTagEnumeration
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getDomainTagEnumeration()
		 * @generated
		 */
		EEnum DOMAIN_TAG_ENUMERATION = eINSTANCE.getDomainTagEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.AttributeTagEnumeration <em>Attribute Tag
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.domain.AttributeTagEnumeration
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getAttributeTagEnumeration()
		 * @generated
		 */
		EEnum ATTRIBUTE_TAG_ENUMERATION = eINSTANCE.getAttributeTagEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.AssociationTagEnumeration <em>Association Tag
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.domain.AssociationTagEnumeration
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getAssociationTagEnumeration()
		 * @generated
		 */
		EEnum ASSOCIATION_TAG_ENUMERATION = eINSTANCE.getAssociationTagEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration <em>Collection Type
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getCollectionTypeEnumeration()
		 * @generated
		 */
		EEnum COLLECTION_TYPE_ENUMERATION = eINSTANCE.getCollectionTypeEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration
		 * <em>Collection Mapping Strategy Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration
		 * @see net.codecadenza.eclipse.model.domain.impl.DomainPackageImpl#getCollectionMappingStrategyEnumeration()
		 * @generated
		 */
		EEnum COLLECTION_MAPPING_STRATEGY_ENUMERATION = eINSTANCE.getCollectionMappingStrategyEnumeration();

	}

}
