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
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Data Exchange Attribute</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getValueListEntries <em>Value List Entries</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isVisible <em>Visible</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isOptional <em>Optional</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getAttributeOrder <em>Attribute Order</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isReadonly <em>Readonly</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getElement <em>Element</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getMappingAttribute <em>Mapping Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getDataType <em>Data Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getFormat <em>Format</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isDisableExternalMapping <em>Disable External
 * Mapping</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isUsedForCustomQuery <em>Used For Custom
 * Query</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute()
 * @model
 * @generated
 */
public interface DataExchangeAttribute extends EObject {
	/**
	 * Return the value of the '<em><b>Value List Entries</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.exchange.ValueListEntry}.
	 * @return the value of the '<em>Value List Entries</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_ValueListEntries()
	 * @model containment="true"
	 * @generated
	 */
	EList<ValueListEntry> getValueListEntries();

	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Visible</b></em>' attribute
	 * @return the value of the '<em>Visible</em>' attribute
	 * @see #setVisible(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Visible()
	 * @model
	 * @generated
	 */
	boolean isVisible();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isVisible <em>Visible</em>}'
	 * attribute
	 * @param value the new value of the '<em>Visible</em>' attribute
	 * @see #isVisible()
	 * @generated
	 */
	void setVisible(boolean value);

	/**
	 * Return the value of the '<em><b>Optional</b></em>' attribute
	 * @return the value of the '<em>Optional</em>' attribute
	 * @see #setOptional(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Optional()
	 * @model
	 * @generated
	 */
	boolean isOptional();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isOptional <em>Optional</em>}'
	 * attribute
	 * @param value the new value of the '<em>Optional</em>' attribute
	 * @see #isOptional()
	 * @generated
	 */
	void setOptional(boolean value);

	/**
	 * Return the value of the '<em><b>Attribute Order</b></em>' attribute
	 * @return the value of the '<em>Attribute Order</em>' attribute
	 * @see #setAttributeOrder(Integer)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_AttributeOrder()
	 * @model
	 * @generated
	 */
	Integer getAttributeOrder();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getAttributeOrder <em>Attribute
	 * Order</em>}' attribute
	 * @param value the new value of the '<em>Attribute Order</em>' attribute
	 * @see #getAttributeOrder()
	 * @generated
	 */
	void setAttributeOrder(Integer value);

	/**
	 * Return the value of the '<em><b>Readonly</b></em>' attribute
	 * @return the value of the '<em>Readonly</em>' attribute
	 * @see #setReadonly(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Readonly()
	 * @model
	 * @generated
	 */
	boolean isReadonly();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isReadonly <em>Readonly</em>}'
	 * attribute
	 * @param value the new value of the '<em>Readonly</em>' attribute
	 * @see #isReadonly()
	 * @generated
	 */
	void setReadonly(boolean value);

	/**
	 * Return the value of the '<em><b>Element</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.exchange.DataExchangeElement#getAttributes <em>Attributes</em>}'.
	 * @return the value of the '<em>Element</em>' container reference
	 * @see #setElement(DataExchangeElement)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Element()
	 * @see net.codecadenza.eclipse.model.exchange.DataExchangeElement#getAttributes
	 * @model opposite="attributes" transient="false"
	 * @generated
	 */
	DataExchangeElement getElement();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getElement <em>Element</em>}'
	 * container reference
	 * @param value the new value of the '<em>Element</em>' container reference
	 * @see #getElement()
	 * @generated
	 */
	void setElement(DataExchangeElement value);

	/**
	 * Return the value of the '<em><b>Mapping Attribute</b></em>' reference
	 * @return the value of the '<em>Mapping Attribute</em>' reference
	 * @see #setMappingAttribute(ExchangeMappingAttribute)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_MappingAttribute()
	 * @model
	 * @generated
	 */
	ExchangeMappingAttribute getMappingAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getMappingAttribute <em>Mapping
	 * Attribute</em>}' reference
	 * @param value the new value of the '<em>Mapping Attribute</em>' reference
	 * @see #getMappingAttribute()
	 * @generated
	 */
	void setMappingAttribute(ExchangeMappingAttribute value);

	/**
	 * Return the value of the '<em><b>Data Type</b></em>' reference
	 * @return the value of the '<em>Data Type</em>' reference
	 * @see #setDataType(JavaType)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_DataType()
	 * @model
	 * @generated
	 */
	JavaType getDataType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getDataType <em>Data Type</em>}'
	 * reference
	 * @param value the new value of the '<em>Data Type</em>' reference
	 * @see #getDataType()
	 * @generated
	 */
	void setDataType(JavaType value);

	/**
	 * Return the value of the '<em><b>Format</b></em>' attribute
	 * @return the value of the '<em>Format</em>' attribute
	 * @see #setFormat(String)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_Format()
	 * @model
	 * @generated
	 */
	String getFormat();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#getFormat <em>Format</em>}'
	 * attribute
	 * @param value the new value of the '<em>Format</em>' attribute
	 * @see #getFormat()
	 * @generated
	 */
	void setFormat(String value);

	/**
	 * Return the value of the '<em><b>Disable External Mapping</b></em>' attribute
	 * @return the value of the '<em>Disable External Mapping</em>' attribute
	 * @see #setDisableExternalMapping(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_DisableExternalMapping()
	 * @model
	 * @generated
	 */
	boolean isDisableExternalMapping();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isDisableExternalMapping
	 * <em>Disable External Mapping</em>}' attribute
	 * @param value the new value of the '<em>Disable External Mapping</em>' attribute
	 * @see #isDisableExternalMapping()
	 * @generated
	 */
	void setDisableExternalMapping(boolean value);

	/**
	 * Return the value of the '<em><b>Used For Custom Query</b></em>' attribute
	 * @return the value of the '<em>Used For Custom Query</em>' attribute
	 * @see #setUsedForCustomQuery(boolean)
	 * @see net.codecadenza.eclipse.model.exchange.ExchangePackage#getDataExchangeAttribute_UsedForCustomQuery()
	 * @model
	 * @generated
	 */
	boolean isUsedForCustomQuery();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.exchange.DataExchangeAttribute#isUsedForCustomQuery <em>Used For
	 * Custom Query</em>}' attribute
	 * @param value the new value of the '<em>Used For Custom Query</em>' attribute
	 * @see #isUsedForCustomQuery()
	 * @generated
	 */
	void setUsedForCustomQuery(boolean value);

}
