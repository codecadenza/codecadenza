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

import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Data Exchange Element</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMinOccurrences <em>Min Occurrences</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMaxOccurrences <em>Max Occurrences</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getElementOrder <em>Element Order</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getTypeName <em>Type Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getWrapperElementName <em>Wrapper Element Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataExchangeMethod <em>Data Exchange Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getValueListEntries <em>Value List Entries</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getAttributes <em>Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMappingAttribute <em>Mapping Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getSubElements <em>Sub Elements</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getParentElement <em>Parent Element</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataType <em>Data Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMappingObject <em>Mapping Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#isContainer <em>Container</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#isDisableExternalMapping <em>Disable External
 * Mapping</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#isUsedForCustomQuery <em>Used For Custom Query</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement()
 * @model
 * @generated
 */
public interface DataExchangeElement extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Min Occurrences</b></em>' attribute
	 * @return the value of the '<em>Min Occurrences</em>' attribute
	 * @see #setMinOccurrences(Integer)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_MinOccurrences()
	 * @model
	 * @generated
	 */
	Integer getMinOccurrences();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMinOccurrences <em>Min
	 * Occurrences</em>}' attribute
	 * @param value the new value of the '<em>Min Occurrences</em>' attribute
	 * @see #getMinOccurrences()
	 * @generated
	 */
	void setMinOccurrences(Integer value);

	/**
	 * Return the value of the '<em><b>Max Occurrences</b></em>' attribute
	 * @return the value of the '<em>Max Occurrences</em>' attribute
	 * @see #setMaxOccurrences(Integer)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_MaxOccurrences()
	 * @model
	 * @generated
	 */
	Integer getMaxOccurrences();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMaxOccurrences <em>Max
	 * Occurrences</em>}' attribute
	 * @param value the new value of the '<em>Max Occurrences</em>' attribute
	 * @see #getMaxOccurrences()
	 * @generated
	 */
	void setMaxOccurrences(Integer value);

	/**
	 * Return the value of the '<em><b>Element Order</b></em>' attribute
	 * @return the value of the '<em>Element Order</em>' attribute
	 * @see #setElementOrder(Integer)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_ElementOrder()
	 * @model
	 * @generated
	 */
	Integer getElementOrder();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getElementOrder <em>Element
	 * Order</em>}' attribute
	 * @param value the new value of the '<em>Element Order</em>' attribute
	 * @see #getElementOrder()
	 * @generated
	 */
	void setElementOrder(Integer value);

	/**
	 * Return the value of the '<em><b>Type Name</b></em>' attribute
	 * @return the value of the '<em>Type Name</em>' attribute
	 * @see #setTypeName(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_TypeName()
	 * @model
	 * @generated
	 */
	String getTypeName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getTypeName <em>Type Name</em>}'
	 * attribute
	 * @param value the new value of the '<em>Type Name</em>' attribute
	 * @see #getTypeName()
	 * @generated
	 */
	void setTypeName(String value);

	/**
	 * Return the value of the '<em><b>Wrapper Element Name</b></em>' attribute
	 * @return the value of the '<em>Wrapper Element Name</em>' attribute
	 * @see #setWrapperElementName(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_WrapperElementName()
	 * @model
	 * @generated
	 */
	String getWrapperElementName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getWrapperElementName <em>Wrapper
	 * Element Name</em>}' attribute
	 * @param value the new value of the '<em>Wrapper Element Name</em>' attribute
	 * @see #getWrapperElementName()
	 * @generated
	 */
	void setWrapperElementName(String value);

	/**
	 * Return the value of the '<em><b>Data Exchange Method</b></em>' container reference. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRootElement <em>Root Element</em>}'.
	 * @return the value of the '<em>Data Exchange Method</em>' container reference
	 * @see #setDataExchangeMethod(DataExchangeMethod)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_DataExchangeMethod()
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeMethod#getRootElement
	 * @model opposite="rootElement" transient="false"
	 * @generated
	 */
	DataExchangeMethod getDataExchangeMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataExchangeMethod <em>Data
	 * Exchange Method</em>}' container reference
	 * @param value the new value of the '<em>Data Exchange Method</em>' container reference
	 * @see #getDataExchangeMethod()
	 * @generated
	 */
	void setDataExchangeMethod(DataExchangeMethod value);

	/**
	 * Return the value of the '<em><b>Value List Entries</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.exchange.ValueListEntry}.
	 * @return the value of the '<em>Value List Entries</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_ValueListEntries()
	 * @model containment="true"
	 * @generated
	 */
	EList<ValueListEntry> getValueListEntries();

	/**
	 * Return the value of the '<em><b>Attributes</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getElement <em>Element</em>}'.
	 * @return the value of the '<em>Attributes</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_Attributes()
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getElement
	 * @model opposite="element" containment="true"
	 * @generated
	 */
	EList<DataExchangeAttribute> getAttributes();

	/**
	 * Return the value of the '<em><b>Mapping Attribute</b></em>' reference
	 * @return the value of the '<em>Mapping Attribute</em>' reference
	 * @see #setMappingAttribute(ExchangeMappingAttribute)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_MappingAttribute()
	 * @model
	 * @generated
	 */
	ExchangeMappingAttribute getMappingAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMappingAttribute <em>Mapping
	 * Attribute</em>}' reference
	 * @param value the new value of the '<em>Mapping Attribute</em>' reference
	 * @see #getMappingAttribute()
	 * @generated
	 */
	void setMappingAttribute(ExchangeMappingAttribute value);

	/**
	 * Return the value of the '<em><b>Sub Elements</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.exchange.DataExchangeElement}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getParentElement <em>Parent Element</em>}'.
	 * @return the value of the '<em>Sub Elements</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_SubElements()
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getParentElement
	 * @model opposite="parentElement" containment="true"
	 * @generated
	 */
	EList<DataExchangeElement> getSubElements();

	/**
	 * Return the value of the '<em><b>Parent Element</b></em>' container reference. It is bidirectional and its opposite is '
	 * {@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getSubElements <em>Sub Elements</em>}'.
	 * @return the value of the '<em>Parent Element</em>' container reference
	 * @see #setParentElement(DataExchangeElement)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_ParentElement()
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getSubElements
	 * @model opposite="subElements" transient="false"
	 * @generated
	 */
	DataExchangeElement getParentElement();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getParentElement <em>Parent
	 * Element</em>}' container reference
	 * @param value the new value of the '<em>Parent Element</em>' container reference
	 * @see #getParentElement()
	 * @generated
	 */
	void setParentElement(DataExchangeElement value);

	/**
	 * Return the value of the '<em><b>Data Type</b></em>' reference
	 * @return the value of the '<em>Data Type</em>' reference
	 * @see #setDataType(JavaType)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_DataType()
	 * @model
	 * @generated
	 */
	JavaType getDataType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getDataType <em>Data Type</em>}'
	 * reference
	 * @param value the new value of the '<em>Data Type</em>' reference
	 * @see #getDataType()
	 * @generated
	 */
	void setDataType(JavaType value);

	/**
	 * Return the value of the '<em><b>Mapping Object</b></em>' reference
	 * @return the value of the '<em>Mapping Object</em>' reference
	 * @see #setMappingObject(ExchangeMappingObject)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_MappingObject()
	 * @model
	 * @generated
	 */
	ExchangeMappingObject getMappingObject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getMappingObject <em>Mapping
	 * Object</em>}' reference
	 * @param value the new value of the '<em>Mapping Object</em>' reference
	 * @see #getMappingObject()
	 * @generated
	 */
	void setMappingObject(ExchangeMappingObject value);

	/**
	 * Return the value of the '<em><b>Container</b></em>' attribute
	 * @return the value of the '<em>Container</em>' attribute
	 * @see #setContainer(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_Container()
	 * @model
	 * @generated
	 */
	boolean isContainer();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#isContainer <em>Container</em>}'
	 * attribute
	 * @param value the new value of the '<em>Container</em>' attribute
	 * @see #isContainer()
	 * @generated
	 */
	void setContainer(boolean value);

	/**
	 * Return the value of the '<em><b>Disable External Mapping</b></em>' attribute
	 * @return the value of the '<em>Disable External Mapping</em>' attribute
	 * @see #setDisableExternalMapping(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_DisableExternalMapping()
	 * @model
	 * @generated
	 */
	boolean isDisableExternalMapping();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#isDisableExternalMapping <em>Disable
	 * External Mapping</em>}' attribute
	 * @param value the new value of the '<em>Disable External Mapping</em>' attribute
	 * @see #isDisableExternalMapping()
	 * @generated
	 */
	void setDisableExternalMapping(boolean value);

	/**
	 * Return the value of the '<em><b>Used For Custom Query</b></em>' attribute
	 * @return the value of the '<em>Used For Custom Query</em>' attribute
	 * @see #setUsedForCustomQuery(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeElement_UsedForCustomQuery()
	 * @model
	 * @generated
	 */
	boolean isUsedForCustomQuery();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#isUsedForCustomQuery <em>Used For
	 * Custom Query</em>}' attribute
	 * @param value the new value of the '<em>Used For Custom Query</em>' attribute
	 * @see #isUsedForCustomQuery()
	 * @generated
	 */
	void setUsedForCustomQuery(boolean value);

	/**
	 * @return a list containing all elements of all sub-elements
	 * @generated not
	 */
	BasicEList<DataExchangeElement> getAllElements();

	/**
	 * @return all attributes of this element including all attributes of all sub-elements
	 * @generated not
	 */
	BasicEList<DataExchangeAttribute> getAllAttributes();

	/**
	 * @return the root element of the hierarchy this element belongs to
	 * @generated not
	 */
	DataExchangeElement getRootElement();

	/**
	 * Perform the validation regarding element and attribute names
	 * @throws IllegalStateException if the given data exchange element (or one of its children) contains duplicate attribute and
	 *           element names
	 * @generated not
	 */
	void checkElementStructure();

}
