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

import java.util.Optional;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.java.JavaType;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Attribute</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isPk <em>Pk</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getLabelPlural <em>Label Plural</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isPersistent <em>Persistent</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isFetchTypeEager <em>Fetch Type Eager</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isInsertable <em>Insertable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isUpdatable <em>Updatable</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isTrackVersion <em>Track Version</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isSetDateOnPersist <em>Set Date On Persist</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isSetDateOnUpdate <em>Set Date On Update</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isDisplayAttribute <em>Display Attribute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainObject <em>Domain Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainAttributeValidator <em>Domain Attribute
 * Validator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getTemporalType <em>Temporal Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getJavaType <em>Java Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getColumn <em>Column</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getTag <em>Tag</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isLob <em>Lob</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getInternalComment <em>Internal Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getUserComment <em>User Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isRemoveWhitespaceCharacters <em>Remove Whitespace
 * Characters</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isConvertToUpperCase <em>Convert To Upper Case</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isConvertToLowerCase <em>Convert To Lower Case</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionType <em>Collection Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionMappingStrategy <em>Collection Mapping
 * Strategy</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute()
 * @model
 * @generated
 */
public interface DomainAttribute extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Pk</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Pk</em>' attribute
	 * @see #setPk(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Pk()
	 * @model default="false"
	 * @generated
	 */
	boolean isPk();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isPk <em>Pk</em>}' attribute
	 * @param value the new value of the '<em>Pk</em>' attribute
	 * @see #isPk()
	 * @generated
	 */
	void setPk(boolean value);

	/**
	 * Return the value of the '<em><b>Label</b></em>' attribute
	 * @return the value of the '<em>Label</em>' attribute
	 * @see #setLabel(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getLabel <em>Label</em>}' attribute
	 * @param value the new value of the '<em>Label</em>' attribute
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

	/**
	 * Return the value of the '<em><b>Label Plural</b></em>' attribute
	 * @return the value of the '<em>Label Plural</em>' attribute
	 * @see #setLabelPlural(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_LabelPlural()
	 * @model
	 * @generated
	 */
	String getLabelPlural();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getLabelPlural <em>Label Plural</em>}'
	 * attribute
	 * @param value the new value of the '<em>Label Plural</em>' attribute
	 * @see #getLabelPlural()
	 * @generated
	 */
	void setLabelPlural(String value);

	/**
	 * Return the value of the '<em><b>Persistent</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Persistent</em>' attribute
	 * @see #setPersistent(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Persistent()
	 * @model default="true"
	 * @generated
	 */
	boolean isPersistent();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isPersistent <em>Persistent</em>}'
	 * attribute
	 * @param value the new value of the '<em>Persistent</em>' attribute
	 * @see #isPersistent()
	 * @generated
	 */
	void setPersistent(boolean value);

	/**
	 * Return the value of the '<em><b>Fetch Type Eager</b></em>' attribute
	 * @return the value of the '<em>Fetch Type Eager</em>' attribute
	 * @see #setFetchTypeEager(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_FetchTypeEager()
	 * @model
	 * @generated
	 */
	boolean isFetchTypeEager();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isFetchTypeEager <em>Fetch Type
	 * Eager</em>}' attribute
	 * @param value the new value of the '<em>Fetch Type Eager</em>' attribute
	 * @see #isFetchTypeEager()
	 * @generated
	 */
	void setFetchTypeEager(boolean value);

	/**
	 * Return the value of the '<em><b>Insertable</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Insertable</em>' attribute
	 * @see #setInsertable(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Insertable()
	 * @model default="true"
	 * @generated
	 */
	boolean isInsertable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isInsertable <em>Insertable</em>}'
	 * attribute
	 * @param value the new value of the '<em>Insertable</em>' attribute
	 * @see #isInsertable()
	 * @generated
	 */
	void setInsertable(boolean value);

	/**
	 * Return the value of the '<em><b>Updatable</b></em>' attribute. The default value is <code>"true"</code>.
	 * @return the value of the '<em>Updatable</em>' attribute
	 * @see #setUpdatable(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Updatable()
	 * @model default="true"
	 * @generated
	 */
	boolean isUpdatable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isUpdatable <em>Updatable</em>}' attribute
	 * @param value the new value of the '<em>Updatable</em>' attribute
	 * @see #isUpdatable()
	 * @generated
	 */
	void setUpdatable(boolean value);

	/**
	 * Return the value of the '<em><b>Track Version</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Track Version</em>' attribute
	 * @see #setTrackVersion(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_TrackVersion()
	 * @model default="false"
	 * @generated
	 */
	boolean isTrackVersion();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isTrackVersion <em>Track Version</em>}'
	 * attribute
	 * @param value the new value of the '<em>Track Version</em>' attribute
	 * @see #isTrackVersion()
	 * @generated
	 */
	void setTrackVersion(boolean value);

	/**
	 * Return the value of the '<em><b>Set Date On Persist</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Set Date On Persist</em>' attribute
	 * @see #setSetDateOnPersist(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_SetDateOnPersist()
	 * @model default="false"
	 * @generated
	 */
	boolean isSetDateOnPersist();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isSetDateOnPersist <em>Set Date On
	 * Persist</em>}' attribute
	 * @param value the new value of the '<em>Set Date On Persist</em>' attribute
	 * @see #isSetDateOnPersist()
	 * @generated
	 */
	void setSetDateOnPersist(boolean value);

	/**
	 * Return the value of the '<em><b>Set Date On Update</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Set Date On Update</em>' attribute
	 * @see #setSetDateOnUpdate(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_SetDateOnUpdate()
	 * @model default="false"
	 * @generated
	 */
	boolean isSetDateOnUpdate();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isSetDateOnUpdate <em>Set Date On
	 * Update</em>}' attribute
	 * @param value the new value of the '<em>Set Date On Update</em>' attribute
	 * @see #isSetDateOnUpdate()
	 * @generated
	 */
	void setSetDateOnUpdate(boolean value);

	/**
	 * Return the value of the '<em><b>Display Attribute</b></em>' attribute. The default value is <code>"false"</code>.
	 * @return the value of the '<em>Display Attribute</em>' attribute
	 * @see #setDisplayAttribute(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_DisplayAttribute()
	 * @model default="false"
	 * @generated
	 */
	boolean isDisplayAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isDisplayAttribute <em>Display
	 * Attribute</em>}' attribute
	 * @param value the new value of the '<em>Display Attribute</em>' attribute
	 * @see #isDisplayAttribute()
	 * @generated
	 */
	void setDisplayAttribute(boolean value);

	/**
	 * Return the value of the '<em><b>Domain Object</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.domain.DomainObject#getAttributes <em>Attributes</em>}'.
	 * @return the value of the '<em>Domain Object</em>' container reference
	 * @see #setDomainObject(DomainObject)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_DomainObject()
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAttributes
	 * @model opposite="attributes"
	 * @generated
	 */
	DomainObject getDomainObject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainObject <em>Domain Object</em>}'
	 * container reference
	 * @param value the new value of the '<em>Domain Object</em>' container reference
	 * @see #getDomainObject()
	 * @generated
	 */
	void setDomainObject(DomainObject value);

	/**
	 * Return the value of the '<em><b>Domain Attribute Validator</b></em>' containment reference
	 * @return the value of the '<em>Domain Attribute Validator</em>' containment reference
	 * @see #setDomainAttributeValidator(DomainAttributeValidator)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_DomainAttributeValidator()
	 * @model containment="true"
	 * @generated
	 */
	DomainAttributeValidator getDomainAttributeValidator();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainAttributeValidator <em>Domain
	 * Attribute Validator</em>}' containment reference
	 * @param value the new value of the '<em>Domain Attribute Validator</em>' containment reference
	 * @see #getDomainAttributeValidator()
	 * @generated
	 */
	void setDomainAttributeValidator(DomainAttributeValidator value);

	/**
	 * Return the value of the '<em><b>Temporal Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration}
	 * @return the value of the '<em>Temporal Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration
	 * @see #setTemporalType(TemporalTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_TemporalType()
	 * @model
	 * @generated
	 */
	TemporalTypeEnumeration getTemporalType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getTemporalType <em>Temporal Type</em>}'
	 * attribute
	 * @param value the new value of the '<em>Temporal Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration
	 * @see #getTemporalType()
	 * @generated
	 */
	void setTemporalType(TemporalTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Java Type</b></em>' reference
	 * @return the value of the '<em>Java Type</em>' reference
	 * @see #setJavaType(JavaType)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_JavaType()
	 * @model
	 * @generated
	 */
	JavaType getJavaType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getJavaType <em>Java Type</em>}' reference
	 * @param value the new value of the '<em>Java Type</em>' reference
	 * @see #getJavaType()
	 * @generated
	 */
	void setJavaType(JavaType value);

	/**
	 * Return the value of the '<em><b>Column</b></em>' reference
	 * @return the value of the '<em>Column</em>' reference
	 * @see #setColumn(DBColumn)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Column()
	 * @model
	 * @generated
	 */
	DBColumn getColumn();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getColumn <em>Column</em>}' reference
	 * @param value the new value of the '<em>Column</em>' reference
	 * @see #getColumn()
	 * @generated
	 */
	void setColumn(DBColumn value);

	/**
	 * Return the value of the '<em><b>Tag</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.domain.AttributeTagEnumeration}.
	 * @return the value of the '<em>Tag</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.AttributeTagEnumeration
	 * @see #setTag(AttributeTagEnumeration)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Tag()
	 * @model
	 * @generated
	 */
	AttributeTagEnumeration getTag();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getTag <em>Tag</em>}' attribute
	 * @param value the new value of the '<em>Tag</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.AttributeTagEnumeration
	 * @see #getTag()
	 * @generated
	 */
	void setTag(AttributeTagEnumeration value);

	/**
	 * Return the value of the '<em><b>Lob</b></em>' attribute
	 * @return the value of the '<em>Lob</em>' attribute
	 * @see #setLob(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_Lob()
	 * @model
	 * @generated
	 */
	boolean isLob();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isLob <em>Lob</em>}' attribute
	 * @param value the new value of the '<em>Lob</em>' attribute
	 * @see #isLob()
	 * @generated
	 */
	void setLob(boolean value);

	/**
	 * Return the value of the '<em><b>Internal Comment</b></em>' attribute
	 * @return the value of the '<em>Internal Comment</em>' attribute
	 * @see #setInternalComment(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_InternalComment()
	 * @model
	 * @generated
	 */
	String getInternalComment();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getInternalComment <em>Internal
	 * Comment</em>}' attribute
	 * @param value the new value of the '<em>Internal Comment</em>' attribute
	 * @see #getInternalComment()
	 * @generated
	 */
	void setInternalComment(String value);

	/**
	 * Return the value of the '<em><b>User Comment</b></em>' attribute
	 * @return the value of the '<em>User Comment</em>' attribute
	 * @see #setUserComment(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_UserComment()
	 * @model
	 * @generated
	 */
	String getUserComment();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getUserComment <em>User Comment</em>}'
	 * attribute
	 * @param value the new value of the '<em>User Comment</em>' attribute
	 * @see #getUserComment()
	 * @generated
	 */
	void setUserComment(String value);

	/**
	 * Return the value of the '<em><b>Remove Whitespace Characters</b></em>' attribute
	 * @return the value of the '<em>Remove Whitespace Characters</em>' attribute
	 * @see #setRemoveWhitespaceCharacters(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_RemoveWhitespaceCharacters()
	 * @model
	 * @generated
	 */
	boolean isRemoveWhitespaceCharacters();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isRemoveWhitespaceCharacters <em>Remove
	 * Whitespace Characters</em>}' attribute
	 * @param value the new value of the '<em>Remove Whitespace Characters</em>' attribute
	 * @see #isRemoveWhitespaceCharacters()
	 * @generated
	 */
	void setRemoveWhitespaceCharacters(boolean value);

	/**
	 * Return the value of the '<em><b>Convert To Upper Case</b></em>' attribute
	 * @return the value of the '<em>Convert To Upper Case</em>' attribute
	 * @see #setConvertToUpperCase(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_ConvertToUpperCase()
	 * @model
	 * @generated
	 */
	boolean isConvertToUpperCase();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isConvertToUpperCase <em>Convert To Upper
	 * Case</em>}' attribute
	 * @param value the new value of the '<em>Convert To Upper Case</em>' attribute
	 * @see #isConvertToUpperCase()
	 * @generated
	 */
	void setConvertToUpperCase(boolean value);

	/**
	 * Return the value of the '<em><b>Convert To Lower Case</b></em>' attribute
	 * @return the value of the '<em>Convert To Lower Case</em>' attribute
	 * @see #setConvertToLowerCase(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_ConvertToLowerCase()
	 * @model
	 * @generated
	 */
	boolean isConvertToLowerCase();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#isConvertToLowerCase <em>Convert To Lower
	 * Case</em>}' attribute
	 * @param value the new value of the '<em>Convert To Lower Case</em>' attribute
	 * @see #isConvertToLowerCase()
	 * @generated
	 */
	void setConvertToLowerCase(boolean value);

	/**
	 * Return the value of the '<em><b>Collection Type</b></em>' attribute
	 * @return the value of the '<em>Collection Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration
	 * @see #setCollectionType(CollectionTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_CollectionType()
	 * @model
	 * @generated
	 */
	CollectionTypeEnumeration getCollectionType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionType <em>Collection
	 * Type</em>}' attribute
	 * @param value the new value of the '<em>Collection Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration
	 * @see #getCollectionType()
	 * @generated
	 */
	void setCollectionType(CollectionTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Collection Mapping Strategy</b></em>' attribute
	 * @return the value of the '<em>Collection Mapping Strategy</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration
	 * @see #setCollectionMappingStrategy(CollectionMappingStrategyEnumeration)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainAttribute_CollectionMappingStrategy()
	 * @model
	 * @generated
	 */
	CollectionMappingStrategyEnumeration getCollectionMappingStrategy();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getCollectionMappingStrategy <em>Collection
	 * Mapping Strategy</em>}' attribute
	 * @param value the new value of the '<em>Collection Mapping Strategy</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration
	 * @see #getCollectionMappingStrategy()
	 * @generated
	 */
	void setCollectionMappingStrategy(CollectionMappingStrategyEnumeration value);

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
	 * @return the default label that is typically used in graphical user interfaces
	 * @generated not
	 */
	String getGUILabel();

	/**
	 * @return true if the attribute value should be converted before being saved
	 * @generated not
	 */
	boolean convertAttribute();

	/**
	 * @return an expression that contains all converters defined for a given domain attribute. An empty string is returned if no
	 *         converter is defined!
	 * @generated not
	 */
	String getConverterExpression();

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
	 * @return the SearchFieldDataTypeEnum based on the attribute's Java type
	 * @throws IllegalStateException if the attribute's type is not supported
	 * @generated not
	 */
	String getSearchFieldDataType();

	/**
	 * @param expression the expression that should be converted
	 * @return a code fragment that converts the given expression to a String
	 * @throws IllegalStateException if the attribute's type is not supported
	 * @generated not
	 */
	String convertToString(String expression);

	/**
	 * @param expression the expression that should be converted
	 * @return a code fragment that converts the given String expression to the attribute's Java type
	 * @throws IllegalStateException if the attribute's type is not supported
	 * @generated not
	 */
	String convertFromString(String expression);

	/**
	 * @param expression the expression that should be converted
	 * @return a code fragment that converts the given expression to a java.time.Instant
	 * @generated not
	 */
	String convertToInstant(String expression);

	/**
	 * @return the default ID value of an empty item
	 * @generated not
	 */
	String getEmptyItemDefaultValue();

	/**
	 * @return true if the domain attribute supports filtering with wildcards (e.g. while performing an auto-complete operation)
	 * @generated not
	 */
	boolean isWildcardFilteringSupported();

	/**
	 * @return the minimum length of this field
	 * @generated not
	 */
	Optional<Integer> getMinFieldLength();

	/**
	 * @return the maximum length of this field
	 * @generated not
	 */
	Optional<Integer> getMaxFieldLenght();

	/**
	 * @return the maximum size of a file that is mapped to this attribute
	 * @generated not
	 */
	String getMaxFileSize();

	/**
	 * @return the full type name
	 * @generated not
	 */
	String getTypeName();

	/**
	 * @return the element collection table or null if this attribute is not mapped to an element collection
	 * @generated not
	 */
	DBTable getCollectionTable();

}
