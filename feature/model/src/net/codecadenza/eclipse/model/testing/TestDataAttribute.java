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
package net.codecadenza.eclipse.model.testing;

import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Test Data Attribute</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getMappingAttribute <em>Mapping Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getValue <em>Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#isTrackValue <em>Track Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getOperator <em>Operator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getReferencedAttribute <em>Referenced Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getReferencedObjects <em>Referenced Objects</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getMappingType <em>Mapping Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSize <em>Expected Size</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSizeOperator <em>Expected Size
 * Operator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getId <em>Id</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute()
 * @model
 * @generated
 */
public interface TestDataAttribute extends EObject {
	String ATTRIBUTE_NAME_FILTER_CRITERIA = "filterCriteria";
	String ATTRIBUTE_NAME_FILTER_OPERATOR = "operator";
	String ATTRIBUTE_NAME_FIELD_NAME = "name";
	String ATTRIBUTE_NAME_SORT_ORDER = "sortOrder";
	String ATTRIBUTE_NAME_SORT_INDEX = "sortIndex";
	String ATTRIBUTE_NAME_IS_DATE_TIME_FORMAT = "dateTimeFormat";
	String ATTRIBUTE_NAME_SEARCH_FIELDS = "searchFields";
	String ATTRIBUTE_NAME_MAX_RESULT = "maxResult";
	String ATTRIBUTE_NAME_NUMBER_FORMAT = "numberFormat";
	String ATTRIBUTE_NAME_DATE_FORMAT = "dateFormat";
	String ATTRIBUTE_NAME_DATE_TIME_FORMAT = "dateTimeFormat";
	String ATTRIBUTE_NAME_DECIMAL_SEPARATOR = "decimalSeparator";
	String ATTRIBUTE_NAME_GROUPING_SEPARATOR = "groupingSeparator";

	/**
	 * Return the value of the '<em><b>Mapping Attribute</b></em>' reference
	 * @return the value of the '<em>Mapping Attribute</em>' reference
	 * @see #setMappingAttribute(MappingAttribute)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_MappingAttribute()
	 * @model
	 * @generated
	 */
	MappingAttribute getMappingAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getMappingAttribute <em>Mapping
	 * Attribute</em>}' reference
	 * @param value the new value of the '<em>Mapping Attribute</em>' reference
	 * @see #getMappingAttribute()
	 * @generated
	 */
	void setMappingAttribute(MappingAttribute value);

	/**
	 * Return the value of the '<em><b>Value</b></em>' attribute
	 * @return the value of the '<em>Value</em>' attribute
	 * @see #setValue(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_Value()
	 * @model
	 * @generated
	 */
	String getValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getValue <em>Value</em>}' attribute
	 * @param value the new value of the '<em>Value</em>' attribute
	 * @see #getValue()
	 * @generated
	 */
	void setValue(String value);

	/**
	 * Return the value of the '<em><b>Track Value</b></em>' attribute
	 * @return the value of the '<em>Track Value</em>' attribute
	 * @see #setTrackValue(boolean)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_TrackValue()
	 * @model
	 * @generated
	 */
	boolean isTrackValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#isTrackValue <em>Track Value</em>}'
	 * attribute
	 * @param value the new value of the '<em>Track Value</em>' attribute
	 * @see #isTrackValue()
	 * @generated
	 */
	void setTrackValue(boolean value);

	/**
	 * Return the value of the '<em><b>Operator</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.testing.AssertionOperator}.
	 * @return the value of the '<em>Operator</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.AssertionOperator
	 * @see #setOperator(AssertionOperator)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_Operator()
	 * @model
	 * @generated
	 */
	AssertionOperator getOperator();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getOperator <em>Operator</em>}'
	 * attribute
	 * @param value the new value of the '<em>Operator</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.AssertionOperator
	 * @see #getOperator()
	 * @generated
	 */
	void setOperator(AssertionOperator value);

	/**
	 * Return the value of the '<em><b>Referenced Attribute</b></em>' reference
	 * @return the value of the '<em>Referenced Attribute</em>' reference
	 * @see #setReferencedAttribute(TestDataAttribute)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_ReferencedAttribute()
	 * @model
	 * @generated
	 */
	TestDataAttribute getReferencedAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getReferencedAttribute <em>Referenced
	 * Attribute</em>}' reference
	 * @param value the new value of the '<em>Referenced Attribute</em>' reference
	 * @see #getReferencedAttribute()
	 * @generated
	 */
	void setReferencedAttribute(TestDataAttribute value);

	/**
	 * Return the value of the '<em><b>Referenced Objects</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.TestDataObject}.
	 * @return the value of the '<em>Referenced Objects</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_ReferencedObjects()
	 * @model containment="true"
	 * @generated
	 */
	EList<TestDataObject> getReferencedObjects();

	/**
	 * Return the value of the '<em><b>Mapping Type</b></em>' reference
	 * @return the value of the '<em>Mapping Type</em>' reference
	 * @see #setMappingType(JavaType)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_MappingType()
	 * @model
	 * @generated
	 */
	JavaType getMappingType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getMappingType <em>Mapping Type</em>}'
	 * reference
	 * @param value the new value of the '<em>Mapping Type</em>' reference
	 * @see #getMappingType()
	 * @generated
	 */
	void setMappingType(JavaType value);

	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Expected Size</b></em>' attribute
	 * @return the value of the '<em>Expected Size</em>' attribute
	 * @see #setExpectedSize(Integer)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_ExpectedSize()
	 * @model
	 * @generated
	 */
	Integer getExpectedSize();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSize <em>Expected Size</em>}'
	 * attribute
	 * @param value the new value of the '<em>Expected Size</em>' attribute
	 * @see #getExpectedSize()
	 * @generated
	 */
	void setExpectedSize(Integer value);

	/**
	 * Return the value of the '<em><b>Expected Size Operator</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.testing.AssertionOperator}.
	 * @return the value of the '<em>Expected Size Operator</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.AssertionOperator
	 * @see #setExpectedSizeOperator(AssertionOperator)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_ExpectedSizeOperator()
	 * @model
	 * @generated
	 */
	AssertionOperator getExpectedSizeOperator();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSizeOperator <em>Expected
	 * Size Operator</em>}' attribute
	 * @param value the new value of the '<em>Expected Size Operator</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.AssertionOperator
	 * @see #getExpectedSizeOperator()
	 * @generated
	 */
	void setExpectedSizeOperator(AssertionOperator value);

	/**
	 * Return the value of the '<em><b>Id</b></em>' attribute
	 * @return the value of the '<em>Id</em>' attribute
	 * @see #setId(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getTestDataAttribute_Id()
	 * @model
	 * @generated
	 */
	String getId();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getId <em>Id</em>}' attribute
	 * @param value the new value of the '<em>Id</em>' attribute
	 * @see #getId()
	 * @generated
	 */
	void setId(String value);

	/**
	 * @return the type of the test data attribute
	 * @generated not
	 */
	JavaType getJavaType();

	/**
	 * @return the label of the test data attribute used in the GUI
	 * @generated not
	 */
	String getLabel();

	/**
	 * @return true if the test data attribute is mandatory
	 * @generated not
	 */
	boolean isMandatory();

	/**
	 * @return true if the attribute can be ignored as the fields like 'operator' or 'value' are not set
	 * @generated not
	 */
	boolean skip();

	/**
	 * @return true if the test data attribute is mapped to an element collection
	 * @generated not
	 */
	boolean isMappedToElementCollection();

	/**
	 * @return true if the attribute represents a path to a file
	 * @generated not
	 */
	boolean isMappedToFile();

	/**
	 * @return true if the attribute is mapped to a list
	 * @generated not
	 */
	boolean isMappedToList();

	/**
	 * Check if this test data attribute is allowed to reference the primary key attribute of the given domain object
	 * @param domainObject
	 * @return true if the reference is allowed to be set
	 * @generated not
	 */
	boolean isReferenceAllowed(DomainObject domainObject);

}
