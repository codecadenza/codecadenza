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
package net.codecadenza.eclipse.model.java;

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
 * @see net.codecadenza.eclipse.model.java.JavaFactory
 * @model kind="package"
 * @generated
 */
public interface JavaPackage extends EPackage {
	/**
	 * The package name
	 * @generated
	 */
	String eNAME = "java";

	/**
	 * The package namespace URI
	 * @generated
	 */
	String eNS_URI = "http:///net/codecadenza/eclipse/model/java.ecore";

	/**
	 * The package namespace name
	 * @generated
	 */
	String eNS_PREFIX = "net.codecadenza.eclipse.model.java";

	/**
	 * The singleton instance of the package
	 * @generated
	 */
	JavaPackage eINSTANCE = net.codecadenza.eclipse.model.java.impl.JavaPackageImpl.init();

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.java.impl.EnumLiteralImpl <em>Enum Literal</em>}' class
	 * @see net.codecadenza.eclipse.model.java.impl.EnumLiteralImpl
	 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getEnumLiteral()
	 * @generated
	 */
	int ENUM_LITERAL = 0;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ENUM_LITERAL__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Tag</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int ENUM_LITERAL__TAG = 1;

	/**
	 * The feature ID for the '<em><b>Java Enum</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int ENUM_LITERAL__JAVA_ENUM = 2;

	/**
	 * The number of structural features of the '<em>Enum Literal</em>' class
	 * @generated
	 * @ordered
	 */
	int ENUM_LITERAL_FEATURE_COUNT = 3;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.java.impl.JavaTypeImpl <em>Java Type</em>}' class
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl
	 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getJavaType()
	 * @generated
	 */
	int JAVA_TYPE = 3;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_TYPE__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_TYPE__COMMENT = 1;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_TYPE__MAPPABLE = 2;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_TYPE__PRIMITIVE = 3;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int JAVA_TYPE__NAMESPACE = 4;

	/**
	 * The number of structural features of the '<em>Java Type</em>' class
	 * @generated
	 * @ordered
	 */
	int JAVA_TYPE_FEATURE_COUNT = 5;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.java.impl.JavaEnumImpl <em>Enum</em>}' class
	 * @see net.codecadenza.eclipse.model.java.impl.JavaEnumImpl
	 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getJavaEnum()
	 * @generated
	 */
	int JAVA_ENUM = 1;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_ENUM__NAME = JAVA_TYPE__NAME;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_ENUM__COMMENT = JAVA_TYPE__COMMENT;

	/**
	 * The feature ID for the '<em><b>Mappable</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_ENUM__MAPPABLE = JAVA_TYPE__MAPPABLE;

	/**
	 * The feature ID for the '<em><b>Primitive</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_ENUM__PRIMITIVE = JAVA_TYPE__PRIMITIVE;

	/**
	 * The feature ID for the '<em><b>Namespace</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int JAVA_ENUM__NAMESPACE = JAVA_TYPE__NAMESPACE;

	/**
	 * The feature ID for the '<em><b>Enumeration Values</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int JAVA_ENUM__ENUMERATION_VALUES = JAVA_TYPE_FEATURE_COUNT + 0;

	/**
	 * The feature ID for the '<em><b>Tag</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_ENUM__TAG = JAVA_TYPE_FEATURE_COUNT + 1;

	/**
	 * The number of structural features of the '<em>Enum</em>' class
	 * @generated
	 * @ordered
	 */
	int JAVA_ENUM_FEATURE_COUNT = JAVA_TYPE_FEATURE_COUNT + 2;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.java.impl.JavaMethodImpl <em>Java Method</em>}' class
	 * @see net.codecadenza.eclipse.model.java.impl.JavaMethodImpl
	 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getJavaMethod()
	 * @generated
	 */
	int JAVA_METHOD = 2;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_METHOD__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Comment</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_METHOD__COMMENT = 1;

	/**
	 * The feature ID for the '<em><b>Java Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int JAVA_METHOD__JAVA_TYPE = 2;

	/**
	 * The feature ID for the '<em><b>Return Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int JAVA_METHOD__RETURN_TYPE = 3;

	/**
	 * The feature ID for the '<em><b>Return Type Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int JAVA_METHOD__RETURN_TYPE_MODIFIER = 4;

	/**
	 * The feature ID for the '<em><b>Method Parameters</b></em>' containment reference list
	 * @generated
	 * @ordered
	 */
	int JAVA_METHOD__METHOD_PARAMETERS = 5;

	/**
	 * The number of structural features of the '<em>Java Method</em>' class
	 * @generated
	 * @ordered
	 */
	int JAVA_METHOD_FEATURE_COUNT = 6;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.java.impl.MethodParameterImpl <em>Method Parameter</em>}'
	 * class
	 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl
	 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getMethodParameter()
	 * @generated
	 */
	int METHOD_PARAMETER = 4;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int METHOD_PARAMETER__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Method</b></em>' container reference
	 * @generated
	 * @ordered
	 */
	int METHOD_PARAMETER__METHOD = 1;

	/**
	 * The feature ID for the '<em><b>Type</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int METHOD_PARAMETER__TYPE = 2;

	/**
	 * The feature ID for the '<em><b>Modifier</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int METHOD_PARAMETER__MODIFIER = 3;

	/**
	 * The feature ID for the '<em><b>Hint</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int METHOD_PARAMETER__HINT = 4;

	/**
	 * The number of structural features of the '<em>Method Parameter</em>' class
	 * @generated
	 * @ordered
	 */
	int METHOD_PARAMETER_FEATURE_COUNT = 5;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.java.impl.NamespaceImpl <em>Namespace</em>}' class
	 * @see net.codecadenza.eclipse.model.java.impl.NamespaceImpl
	 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getNamespace()
	 * @generated
	 */
	int NAMESPACE = 5;

	/**
	 * The feature ID for the '<em><b>Name</b></em>' attribute
	 * @generated
	 * @ordered
	 */
	int NAMESPACE__NAME = 0;

	/**
	 * The feature ID for the '<em><b>Parent</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int NAMESPACE__PARENT = 1;

	/**
	 * The feature ID for the '<em><b>Child Namespaces</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int NAMESPACE__CHILD_NAMESPACES = 2;

	/**
	 * The feature ID for the '<em><b>Java Types</b></em>' reference list
	 * @generated
	 * @ordered
	 */
	int NAMESPACE__JAVA_TYPES = 3;

	/**
	 * The feature ID for the '<em><b>Project</b></em>' reference
	 * @generated
	 * @ordered
	 */
	int NAMESPACE__PROJECT = 4;

	/**
	 * The number of structural features of the '<em>Namespace</em>' class
	 * @generated
	 * @ordered
	 */
	int NAMESPACE_FEATURE_COUNT = 5;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration <em>Type Modifier
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration
	 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getJavaTypeModifierEnumeration()
	 * @generated
	 */
	int JAVA_TYPE_MODIFIER_ENUMERATION = 6;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration <em>Enum Literal Tag
	 * Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration
	 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getEnumLiteralTagEnumeration()
	 * @generated
	 */
	int ENUM_LITERAL_TAG_ENUMERATION = 7;

	/**
	 * The meta object ID for the '{@link net.codecadenza.eclipse.model.java.EnumTagEnumeration <em>Enum Tag Enumeration</em>}' enum
	 * @see net.codecadenza.eclipse.model.java.EnumTagEnumeration
	 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getEnumTagEnumeration()
	 * @generated
	 */
	int ENUM_TAG_ENUMERATION = 8;

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.java.EnumLiteral <em>Enum Literal</em>}'
	 * @return the meta object for class '<em>Enum Literal</em>'
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral
	 * @generated
	 */
	EClass getEnumLiteral();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.EnumLiteral#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral#getName()
	 * @see #getEnumLiteral()
	 * @generated
	 */
	EAttribute getEnumLiteral_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.EnumLiteral#getTag <em>Tag</em>}'
	 * @return the meta object for the attribute '<em>Tag</em>'
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral#getTag()
	 * @see #getEnumLiteral()
	 * @generated
	 */
	EAttribute getEnumLiteral_Tag();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.java.EnumLiteral#getJavaEnum
	 * <em>Java Enum</em>}'
	 * @return the meta object for the container reference '<em>Java Enum</em>'
	 * @see net.codecadenza.eclipse.model.java.EnumLiteral#getJavaEnum()
	 * @see #getEnumLiteral()
	 * @generated
	 */
	EReference getEnumLiteral_JavaEnum();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.java.JavaEnum <em>Enum</em>}'
	 * @return the meta object for class '<em>Enum</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaEnum
	 * @generated
	 */
	EClass getJavaEnum();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.java.JavaEnum#getEnumerationValues <em>Enumeration Values</em>}'
	 * @return the meta object for the containment reference list '<em>Enumeration Values</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaEnum#getEnumerationValues()
	 * @see #getJavaEnum()
	 * @generated
	 */
	EReference getJavaEnum_EnumerationValues();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.JavaEnum#getTag <em>Tag</em>}'
	 * @return the meta object for the attribute '<em>Tag</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaEnum#getTag()
	 * @see #getJavaEnum()
	 * @generated
	 */
	EAttribute getJavaEnum_Tag();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.java.JavaMethod <em>Java Method</em>}'
	 * @return the meta object for class '<em>Java Method</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaMethod
	 * @generated
	 */
	EClass getJavaMethod();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.JavaMethod#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getName()
	 * @see #getJavaMethod()
	 * @generated
	 */
	EAttribute getJavaMethod_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.JavaMethod#getComment <em>Comment</em>}'
	 * @return the meta object for the attribute '<em>Comment</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getComment()
	 * @see #getJavaMethod()
	 * @generated
	 */
	EAttribute getJavaMethod_Comment();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.java.JavaMethod#getJavaType <em>Java
	 * Type</em>}'
	 * @return the meta object for the reference '<em>Java Type</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getJavaType()
	 * @see #getJavaMethod()
	 * @generated
	 */
	EReference getJavaMethod_JavaType();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.java.JavaMethod#getReturnType <em>Return
	 * Type</em>}'
	 * @return the meta object for the reference '<em>Return Type</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getReturnType()
	 * @see #getJavaMethod()
	 * @generated
	 */
	EReference getJavaMethod_ReturnType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.JavaMethod#getReturnTypeModifier
	 * <em>Return Type Modifier</em>}'
	 * @return the meta object for the attribute '<em>Return Type Modifier</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getReturnTypeModifier()
	 * @see #getJavaMethod()
	 * @generated
	 */
	EAttribute getJavaMethod_ReturnTypeModifier();

	/**
	 * Return the meta object for the containment reference list
	 * '{@link net.codecadenza.eclipse.model.java.JavaMethod#getMethodParameters <em>Method Parameters</em>}'
	 * @return the meta object for the containment reference list '<em>Method Parameters</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaMethod#getMethodParameters()
	 * @see #getJavaMethod()
	 * @generated
	 */
	EReference getJavaMethod_MethodParameters();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.java.JavaType <em>Java Type</em>}'
	 * @return the meta object for class '<em>Java Type</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaType
	 * @generated
	 */
	EClass getJavaType();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.JavaType#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaType#getName()
	 * @see #getJavaType()
	 * @generated
	 */
	EAttribute getJavaType_Name();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.JavaType#getComment <em>Comment</em>}'
	 * @return the meta object for the attribute '<em>Comment</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaType#getComment()
	 * @see #getJavaType()
	 * @generated
	 */
	EAttribute getJavaType_Comment();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.JavaType#isMappable <em>Mappable</em>}'
	 * @return the meta object for the attribute '<em>Mappable</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaType#isMappable()
	 * @see #getJavaType()
	 * @generated
	 */
	EAttribute getJavaType_Mappable();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.JavaType#isPrimitive <em>Primitive</em>}'
	 * @return the meta object for the attribute '<em>Primitive</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaType#isPrimitive()
	 * @see #getJavaType()
	 * @generated
	 */
	EAttribute getJavaType_Primitive();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.java.JavaType#getNamespace
	 * <em>Namespace</em>}'
	 * @return the meta object for the reference '<em>Namespace</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaType#getNamespace()
	 * @see #getJavaType()
	 * @generated
	 */
	EReference getJavaType_Namespace();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.java.MethodParameter <em>Method Parameter</em>}'
	 * @return the meta object for class '<em>Method Parameter</em>'
	 * @see net.codecadenza.eclipse.model.java.MethodParameter
	 * @generated
	 */
	EClass getMethodParameter();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.MethodParameter#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getName()
	 * @see #getMethodParameter()
	 * @generated
	 */
	EAttribute getMethodParameter_Name();

	/**
	 * Return the meta object for the container reference '{@link net.codecadenza.eclipse.model.java.MethodParameter#getMethod
	 * <em>Method</em>}'
	 * @return the meta object for the container reference '<em>Method</em>'
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getMethod()
	 * @see #getMethodParameter()
	 * @generated
	 */
	EReference getMethodParameter_Method();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.java.MethodParameter#getType <em>Java
	 * Type</em>}'
	 * @return the meta object for the reference '<em>Java Type</em>'
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getType()
	 * @see #getMethodParameter()
	 * @generated
	 */
	EReference getMethodParameter_Type();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.MethodParameter#getModifier
	 * <em>Modifier</em>}'
	 * @return the meta object for the attribute '<em>Modifier</em>'
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getModifier()
	 * @see #getMethodParameter()
	 * @generated
	 */
	EAttribute getMethodParameter_Modifier();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.MethodParameter#getHint <em>Hint</em>}'
	 * @return the meta object for the attribute '<em>Hint</em>'
	 * @see net.codecadenza.eclipse.model.java.MethodParameter#getHint()
	 * @see #getMethodParameter()
	 * @generated
	 */
	EAttribute getMethodParameter_Hint();

	/**
	 * Return the meta object for class '{@link net.codecadenza.eclipse.model.java.Namespace <em>Namespace</em>}'
	 * @return the meta object for class '<em>Namespace</em>'
	 * @see net.codecadenza.eclipse.model.java.Namespace
	 * @generated
	 */
	EClass getNamespace();

	/**
	 * Return the meta object for the attribute '{@link net.codecadenza.eclipse.model.java.Namespace#getName <em>Name</em>}'
	 * @return the meta object for the attribute '<em>Name</em>'
	 * @see net.codecadenza.eclipse.model.java.Namespace#getName()
	 * @see #getNamespace()
	 * @generated
	 */
	EAttribute getNamespace_Name();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.java.Namespace#getParent <em>Parent</em>}'
	 * @return the meta object for the reference '<em>Parent</em>'
	 * @see net.codecadenza.eclipse.model.java.Namespace#getParent()
	 * @see #getNamespace()
	 * @generated
	 */
	EReference getNamespace_Parent();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.java.Namespace#getChildNamespaces
	 * <em>Child Namespaces</em>}'
	 * @return the meta object for the reference list '<em>Child Namespaces</em>'
	 * @see net.codecadenza.eclipse.model.java.Namespace#getChildNamespaces()
	 * @see #getNamespace()
	 * @generated
	 */
	EReference getNamespace_ChildNamespaces();

	/**
	 * Return the meta object for the reference list '{@link net.codecadenza.eclipse.model.java.Namespace#getJavaTypes <em>Java
	 * Types</em>}'
	 * @return the meta object for the reference list '<em>Java Types</em>'
	 * @see net.codecadenza.eclipse.model.java.Namespace#getJavaTypes()
	 * @see #getNamespace()
	 * @generated
	 */
	EReference getNamespace_JavaTypes();

	/**
	 * Return the meta object for the reference '{@link net.codecadenza.eclipse.model.java.Namespace#getProject <em>Project</em>}'
	 * @return the meta object for the reference '<em>Project</em>'
	 * @see net.codecadenza.eclipse.model.java.Namespace#getProject()
	 * @see #getNamespace()
	 * @generated
	 */
	EReference getNamespace_Project();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration <em>Type Modifier
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Type Modifier Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration
	 * @generated
	 */
	EEnum getJavaTypeModifierEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration <em>Enum Literal Tag
	 * Enumeration</em>}'
	 * @return the meta object for enum '<em>Enum Literal Tag Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration
	 * @generated
	 */
	EEnum getEnumLiteralTagEnumeration();

	/**
	 * Return the meta object for enum '{@link net.codecadenza.eclipse.model.java.EnumTagEnumeration <em>Enum Tag Enumeration</em>}'
	 * @return the meta object for enum '<em>Enum Tag Enumeration</em>'
	 * @see net.codecadenza.eclipse.model.java.EnumTagEnumeration
	 * @generated
	 */
	EEnum getEnumTagEnumeration();

	/**
	 * @return the factory that creates the instances of the model
	 * @generated
	 */
	JavaFactory getJavaFactory();

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
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.java.impl.EnumLiteralImpl <em>Enum Literal</em>}'
		 * class
		 * @see net.codecadenza.eclipse.model.java.impl.EnumLiteralImpl
		 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getEnumLiteral()
		 * @generated
		 */
		EClass ENUM_LITERAL = eINSTANCE.getEnumLiteral();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ENUM_LITERAL__NAME = eINSTANCE.getEnumLiteral_Name();

		/**
		 * The meta object literal for the '<em><b>Tag</b></em>' attribute feature
		 * @generated
		 */
		EAttribute ENUM_LITERAL__TAG = eINSTANCE.getEnumLiteral_Tag();

		/**
		 * The meta object literal for the '<em><b>Java Enum</b></em>' container reference feature
		 * @generated
		 */
		EReference ENUM_LITERAL__JAVA_ENUM = eINSTANCE.getEnumLiteral_JavaEnum();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.java.impl.JavaEnumImpl <em>Enum</em>}' class
		 * @see net.codecadenza.eclipse.model.java.impl.JavaEnumImpl
		 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getJavaEnum()
		 * @generated
		 */
		EClass JAVA_ENUM = eINSTANCE.getJavaEnum();

		/**
		 * The meta object literal for the '<em><b>Enumeration Values</b></em>' containment reference list feature
		 * @generated
		 */
		EReference JAVA_ENUM__ENUMERATION_VALUES = eINSTANCE.getJavaEnum_EnumerationValues();

		/**
		 * The meta object literal for the '<em><b>Tag</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JAVA_ENUM__TAG = eINSTANCE.getJavaEnum_Tag();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.java.impl.JavaMethodImpl <em>Java Method</em>}' class
		 * @see net.codecadenza.eclipse.model.java.impl.JavaMethodImpl
		 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getJavaMethod()
		 * @generated
		 */
		EClass JAVA_METHOD = eINSTANCE.getJavaMethod();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JAVA_METHOD__NAME = eINSTANCE.getJavaMethod_Name();

		/**
		 * The meta object literal for the '<em><b>Comment</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JAVA_METHOD__COMMENT = eINSTANCE.getJavaMethod_Comment();

		/**
		 * The meta object literal for the '<em><b>Java Type</b></em>' reference feature
		 * @generated
		 */
		EReference JAVA_METHOD__JAVA_TYPE = eINSTANCE.getJavaMethod_JavaType();

		/**
		 * The meta object literal for the '<em><b>Return Type</b></em>' reference feature
		 * @generated
		 */
		EReference JAVA_METHOD__RETURN_TYPE = eINSTANCE.getJavaMethod_ReturnType();

		/**
		 * The meta object literal for the '<em><b>Return Type Modifier</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JAVA_METHOD__RETURN_TYPE_MODIFIER = eINSTANCE.getJavaMethod_ReturnTypeModifier();

		/**
		 * The meta object literal for the '<em><b>Method Parameters</b></em>' containment reference list feature
		 * @generated
		 */
		EReference JAVA_METHOD__METHOD_PARAMETERS = eINSTANCE.getJavaMethod_MethodParameters();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.java.impl.JavaTypeImpl <em>Java Type</em>}' class
		 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl
		 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getJavaType()
		 * @generated
		 */
		EClass JAVA_TYPE = eINSTANCE.getJavaType();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JAVA_TYPE__NAME = eINSTANCE.getJavaType_Name();

		/**
		 * The meta object literal for the '<em><b>Comment</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JAVA_TYPE__COMMENT = eINSTANCE.getJavaType_Comment();

		/**
		 * The meta object literal for the '<em><b>Mappable</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JAVA_TYPE__MAPPABLE = eINSTANCE.getJavaType_Mappable();

		/**
		 * The meta object literal for the '<em><b>Primitive</b></em>' attribute feature
		 * @generated
		 */
		EAttribute JAVA_TYPE__PRIMITIVE = eINSTANCE.getJavaType_Primitive();

		/**
		 * The meta object literal for the '<em><b>Namespace</b></em>' reference feature
		 * @generated
		 */
		EReference JAVA_TYPE__NAMESPACE = eINSTANCE.getJavaType_Namespace();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.java.impl.MethodParameterImpl <em>Method
		 * Parameter</em>}' class
		 * @see net.codecadenza.eclipse.model.java.impl.MethodParameterImpl
		 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getMethodParameter()
		 * @generated
		 */
		EClass METHOD_PARAMETER = eINSTANCE.getMethodParameter();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute METHOD_PARAMETER__NAME = eINSTANCE.getMethodParameter_Name();

		/**
		 * The meta object literal for the '<em><b>Method</b></em>' container reference feature
		 * @generated
		 */
		EReference METHOD_PARAMETER__METHOD = eINSTANCE.getMethodParameter_Method();

		/**
		 * The meta object literal for the '<em><b>Type</b></em>' reference feature
		 * @generated
		 */
		EReference METHOD_PARAMETER__TYPE = eINSTANCE.getMethodParameter_Type();

		/**
		 * The meta object literal for the '<em><b>Modifier</b></em>' attribute feature
		 * @generated
		 */
		EAttribute METHOD_PARAMETER__MODIFIER = eINSTANCE.getMethodParameter_Modifier();

		/**
		 * The meta object literal for the '<em><b>Hint</b></em>' attribute feature
		 * @generated
		 */
		EAttribute METHOD_PARAMETER__HINT = eINSTANCE.getMethodParameter_Hint();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.java.impl.NamespaceImpl <em>Namespace</em>}' class
		 * @see net.codecadenza.eclipse.model.java.impl.NamespaceImpl
		 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getNamespace()
		 * @generated
		 */
		EClass NAMESPACE = eINSTANCE.getNamespace();

		/**
		 * The meta object literal for the '<em><b>Name</b></em>' attribute feature
		 * @generated
		 */
		EAttribute NAMESPACE__NAME = eINSTANCE.getNamespace_Name();

		/**
		 * The meta object literal for the '<em><b>Parent</b></em>' reference feature
		 * @generated
		 */
		EReference NAMESPACE__PARENT = eINSTANCE.getNamespace_Parent();

		/**
		 * The meta object literal for the '<em><b>Child Namespaces</b></em>' reference list feature
		 * @generated
		 */
		EReference NAMESPACE__CHILD_NAMESPACES = eINSTANCE.getNamespace_ChildNamespaces();

		/**
		 * The meta object literal for the '<em><b>Java Types</b></em>' reference list feature
		 * @generated
		 */
		EReference NAMESPACE__JAVA_TYPES = eINSTANCE.getNamespace_JavaTypes();

		/**
		 * The meta object literal for the '<em><b>Project</b></em>' reference feature
		 * @generated
		 */
		EReference NAMESPACE__PROJECT = eINSTANCE.getNamespace_Project();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration <em>Type Modifier
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration
		 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getJavaTypeModifierEnumeration()
		 * @generated
		 */
		EEnum JAVA_TYPE_MODIFIER_ENUMERATION = eINSTANCE.getJavaTypeModifierEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration <em>Enum Literal Tag
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.java.EnumLiteralTagEnumeration
		 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getEnumLiteralTagEnumeration()
		 * @generated
		 */
		EEnum ENUM_LITERAL_TAG_ENUMERATION = eINSTANCE.getEnumLiteralTagEnumeration();

		/**
		 * The meta object literal for the '{@link net.codecadenza.eclipse.model.java.EnumTagEnumeration <em>Enum Tag
		 * Enumeration</em>}' enum
		 * @see net.codecadenza.eclipse.model.java.EnumTagEnumeration
		 * @see net.codecadenza.eclipse.model.java.impl.JavaPackageImpl#getEnumTagEnumeration()
		 * @generated
		 */
		EEnum ENUM_TAG_ENUMERATION = eINSTANCE.getEnumTagEnumeration();

	}

}
