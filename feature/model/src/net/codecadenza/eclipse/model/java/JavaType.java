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

import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.project.Project;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Java Type</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaType#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaType#getComment <em>Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaType#isMappable <em>Mappable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaType#isPrimitive <em>Primitive</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.java.JavaType#getNamespace <em>Namespace</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType()
 * @model
 * @generated
 */
public interface JavaType extends EObject {
	String STRING = "String";
	String DATE = "Date";
	String GREGORIAN_CAL = "GregorianCalendar";
	String INT = "int";
	String INTEGER = "Integer";
	String CHAR = "char";
	String CHARACTER = "Character";
	String DOUBLE = "double";
	String DOUBLE_OBJ = "Double";
	String FLOAT = "float";
	String FLOAT_OBJ = "Float";
	String BOOL = "boolean";
	String BOOL_OBJ = "Boolean";
	String LONG = "long";
	String LONG_OBJ = "Long";
	String VOID = "void";
	String VOID_OBJ = "Void";
	String HASH_MAP = "HashMap";
	String BYTE_ARRAY = "byte[]";
	String BYTE_OBJ_ARRAY = "Byte[]";
	String BIG_DECIMAL = "BigDecimal";
	String LOCAL_DATE = "LocalDate";
	String LOCAL_DATE_TIME = "LocalDateTime";
	String UUID = "UUID";

	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaType#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Comment</b></em>' attribute
	 * @return the value of the '<em>Comment</em>' attribute
	 * @see #setComment(String)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType_Comment()
	 * @model
	 * @generated
	 */
	String getComment();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaType#getComment <em>Comment</em>}' attribute
	 * @param value the new value of the '<em>Comment</em>' attribute
	 * @see #getComment()
	 * @generated
	 */
	void setComment(String value);

	/**
	 * Return the value of the '<em><b>Mappable</b></em>' attribute
	 * @return the value of the '<em>Mappable</em>' attribute
	 * @see #setMappable(boolean)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType_Mappable()
	 * @model
	 * @generated
	 */
	boolean isMappable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaType#isMappable <em>Mappable</em>}' attribute
	 * @param value the new value of the '<em>Mappable</em>' attribute
	 * @see #isMappable()
	 * @generated
	 */
	void setMappable(boolean value);

	/**
	 * Return the value of the '<em><b>Primitive</b></em>' attribute
	 * @return the value of the '<em>Primitive</em>' attribute
	 * @see #setPrimitive(boolean)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType_Primitive()
	 * @model
	 * @generated
	 */
	boolean isPrimitive();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaType#isPrimitive <em>Primitive</em>}' attribute
	 * @param value the new value of the '<em>Primitive</em>' attribute
	 * @see #isPrimitive()
	 * @generated
	 */
	void setPrimitive(boolean value);

	/**
	 * Return the value of the '<em><b>Namespace</b></em>' reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.java.Namespace#getJavaTypes <em>Java Types</em>}'.
	 * @return the value of the '<em>Namespace</em>' reference
	 * @see #setNamespace(Namespace)
	 * @see net.codecadenza.eclipse.model.java.JavaPackage#getJavaType_Namespace()
	 * @see net.codecadenza.eclipse.model.java.Namespace#getJavaTypes
	 * @model opposite="javaTypes"
	 * @generated
	 */
	Namespace getNamespace();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.java.JavaType#getNamespace <em>Namespace</em>}' reference
	 * @param value the new value of the '<em>Namespace</em>' reference
	 * @see #getNamespace()
	 * @generated
	 */
	void setNamespace(Namespace value);

	/**
	 * @return the upper-case name
	 * @generated not
	 */
	String getUpperCaseName();

	/**
	 * @return the lower-case name
	 * @generated not
	 */
	String getLowerCaseName();

	/**
	 * @return a list containing all database types that can be mapped to this Java type
	 * @param project
	 * @generated not
	 */
	EList<DBColumnType> getDBColumnTypes(Project project);

	/**
	 * @param typeNames
	 * @return true if the type name is equal to one of the given names
	 * @generated not
	 */
	boolean isType(String... typeNames);

	/**
	 * @return true if the type is a decimal number (float, Float, double, Double or BigDecimal)
	 * @generated not
	 */
	boolean isDecimalNumber();

	/**
	 * @return true if the type is int, Integer, long or Long
	 * @generated not
	 */
	boolean isIntegerOrLong();

	/**
	 * @return true if the type is a number (int, Integer, long, Long, float, Float, double, Double or BigDecimal)
	 * @generated not
	 */
	boolean isNumber();

	/**
	 * @return true if the type is java.lang.String
	 * @generated not
	 */
	boolean isString();

	/**
	 * @return true if the type is either int or java.lang.Integer
	 * @generated not
	 */
	boolean isInteger();

	/**
	 * @return true if the type is either long or java.lang.Long
	 * @generated not
	 */
	boolean isLong();

	/**
	 * @return true if the type is either float or java.lang.Float
	 * @generated not
	 */
	boolean isFloat();

	/**
	 * @return true if the type is either double or java.lang.Double
	 * @generated not
	 */
	boolean isDouble();

	/**
	 * @return true if the type is java.math.BigDecimal
	 * @generated not
	 */
	boolean isBigDecimal();

	/**
	 * @return true if the type is either boolean or java.lang.Boolean
	 * @generated not
	 */
	boolean isBoolean();

	/**
	 * @return true if the type is char
	 * @generated not
	 */
	boolean isChar();

	/**
	 * @return true if the type is either byte[] or java.lang.Byte[]
	 * @generated not
	 */
	boolean isByteArray();

	/**
	 * @return true if the type is either java.util.Date or java.util.GregorianCalendar
	 * @generated not
	 */
	boolean isDateOrCalendar();

	/**
	 * @return true if the type is java.util.Date
	 * @generated not
	 */
	boolean isDate();

	/**
	 * @return true if the type is java.util.GregorianCalendar
	 * @generated not
	 */
	boolean isCalendar();

	/**
	 * @return true if the type is java.time.LocalDate
	 * @generated not
	 */
	boolean isLocalDate();

	/**
	 * @return true if the type is java.time.LocalDateTime
	 * @generated not
	 */
	boolean isLocalDateTime();

	/**
	 * @return true if the type is either java.util.Date, java.util.GregorianCalendar, java.time.LocalDate or
	 *         java.time.LocalDateTime
	 * @generated not
	 */
	boolean isTemporalType();

	/**
	 * @return true if the type is void
	 * @generated not
	 */
	boolean isVoid();

	/**
	 * @return true if the type is java.util.UUID
	 * @generated not
	 */
	boolean isUUID();

	/**
	 * @return true if the type is an enum
	 * @generated not
	 */
	boolean isEnum();

	/**
	 * @return the name of this type or the name of the respective primitive wrapper type
	 * @generated not
	 */
	String getWrapperTypeName();

	/**
	 * @return the default value of a local variable for this type
	 * @generated not
	 */
	String getLocalVariableDefaultValue();

}
