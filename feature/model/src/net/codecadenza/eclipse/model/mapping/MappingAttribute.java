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
package net.codecadenza.eclipse.model.mapping;

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Attribute</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociation <em>Association</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getDomainAttribute <em>Domain Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociationList <em>Association List</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getMappingType <em>Mapping Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getModifier <em>Modifier</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getDefaultValue <em>Default Value</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute()
 * @model abstract="true"
 * @generated
 */
public interface MappingAttribute extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Association</b></em>' reference
	 * @return the value of the '<em>Association</em>' reference
	 * @see #setAssociation(AbstractDomainAssociation)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_Association()
	 * @model
	 * @generated
	 */
	AbstractDomainAssociation getAssociation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getAssociation <em>Association</em>}'
	 * reference
	 * @param value the new value of the '<em>Association</em>' reference
	 * @see #getAssociation()
	 * @generated
	 */
	void setAssociation(AbstractDomainAssociation value);

	/**
	 * Return the value of the '<em><b>Domain Attribute</b></em>' reference
	 * @return the value of the '<em>Domain Attribute</em>' reference
	 * @see #setDomainAttribute(DomainAttribute)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_DomainAttribute()
	 * @model
	 * @generated
	 */
	DomainAttribute getDomainAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getDomainAttribute <em>Domain
	 * Attribute</em>}' reference
	 * @param value the new value of the '<em>Domain Attribute</em>' reference
	 * @see #getDomainAttribute()
	 * @generated
	 */
	void setDomainAttribute(DomainAttribute value);

	/**
	 * Return the value of the '<em><b>Association List</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation}.
	 * @return the value of the '<em>Association List</em>' reference list
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_AssociationList()
	 * @model
	 * @generated
	 */
	EList<AbstractDomainAssociation> getAssociationList();

	/**
	 * Return the value of the '<em><b>Mapping Type</b></em>' reference
	 * @return the value of the '<em>Mapping Type</em>' reference
	 * @see #setMappingType(JavaType)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_MappingType()
	 * @model
	 * @generated
	 */
	JavaType getMappingType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getMappingType <em>Mapping Type</em>}'
	 * reference
	 * @param value the new value of the '<em>Mapping Type</em>' reference
	 * @see #getMappingType()
	 * @generated
	 */
	void setMappingType(JavaType value);

	/**
	 * Return the value of the '<em><b>Modifier</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration}.
	 * @return the value of the '<em>Modifier</em>' attribute
	 * @see net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration
	 * @see #setModifier(JavaTypeModifierEnumeration)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_Modifier()
	 * @model
	 * @generated
	 */
	JavaTypeModifierEnumeration getModifier();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getModifier <em>Modifier</em>}' attribute
	 * @param value the new value of the '<em>Modifier</em>' attribute
	 * @see net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration
	 * @see #getModifier()
	 * @generated
	 */
	void setModifier(JavaTypeModifierEnumeration value);

	/**
	 * Return the value of the '<em><b>Default Value</b></em>' attribute
	 * @return the value of the '<em>Default Value</em>' attribute
	 * @see #setDefaultValue(String)
	 * @see net.codecadenza.eclipse.model.mapping.MappingPackage#getMappingAttribute_DefaultValue()
	 * @model
	 * @generated
	 */
	String getDefaultValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.mapping.MappingAttribute#getDefaultValue <em>Default Value</em>}'
	 * attribute
	 * @param value the new value of the '<em>Default Value</em>' attribute
	 * @see #getDefaultValue()
	 * @generated
	 */
	void setDefaultValue(String value);

	/**
	 * @return the upper-case name of this attribute (e.g. Value)
	 * @generated not
	 */
	String getUpperCaseName();

	/**
	 * @return the name of the getter (e.g. getValue())
	 * @generated not
	 */
	String getGetterName();

	/**
	 * @return the name of the setter (e.g. setValue)
	 * @generated not
	 */
	String getSetterName();

	/**
	 * @return the method reference of the getter (e.g. ::getValue)
	 * @generated not
	 */
	String getGetterReference();

	/**
	 * @return the method reference of the setter (e.g. ::setValue)
	 * @generated not
	 */
	String getSetterReference();

	/**
	 * @return a fully cascaded getter of all entries that are contained in the association list of this attribute. An empty string
	 *         will be returned if the association list is null or empty!
	 * @generated not
	 */
	String getAssociationListGetter();

	/**
	 * Create an if-statement that checks all optional associations of the association list
	 * @param objectName the name of the object that contains the attribute
	 * @param closeStatement a flag that controls if the generated if-statement should be closed with a bracket
	 * @return the generated if-statement
	 * @generated not
	 */
	String getAssociationListNullCheck(String objectName, boolean closeStatement);

	/**
	 * Create an if-statement that checks all optional associations so that the respective attribute can be safely accessed
	 * @param objectName the name of the object that contains the attribute
	 * @param additionalCondition an additional condition that will be added at the end of the if-statement
	 * @return the generated if-statement
	 * @generated not
	 */
	String getNullCheck(String objectName, String additionalCondition);

	/**
	 * Get the Java type that should be used when mapping a field in a query. In the case of outer joins it is possible that a
	 * primitive field of an object has no value as the respective association is null. Depending on the persistence provider such a
	 * field is either filled with a default value or on exception is thrown. Therefore, this method returns the corresponding
	 * wrapper type if necessary.
	 * @return either the Java type this field is mapped to or the corresponding wrapper type
	 * @generated not
	 */
	JavaType getSearchType();

}
