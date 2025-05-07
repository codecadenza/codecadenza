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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Domain Object</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getLabel <em>Label</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getNamePlural <em>Name Plural</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getLabelPlural <em>Label Plural</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorValue <em>Discriminator Value</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorColumnType <em>Discriminator Column
 * Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getInheritanceType <em>Inheritance Type</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#isPropertyAccess <em>Property Access</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#isAbstract <em>Abstract</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#isMappedSuperClass <em>Mapped Super Class</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getParent <em>Parent</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getIDGenerator <em>ID Generator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getInheritance <em>Inheritance</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getAttributes <em>Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getAssociations <em>Associations</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getEnumAssociations <em>Enum Associations</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getTargetInheritances <em>Target Inheritances</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorColumn <em>Discriminator Column</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getDatabaseTable <em>Database Table</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainObject#getTag <em>Tag</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject()
 * @model
 * @generated
 */
public interface DomainObject extends JavaType {
	/**
	 * Return the value of the '<em><b>Label</b></em>' attribute
	 * @return the value of the '<em>Label</em>' attribute
	 * @see #setLabel(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Label()
	 * @model
	 * @generated
	 */
	String getLabel();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getLabel <em>Label</em>}' attribute
	 * @param value the new value of the '<em>Label</em>' attribute
	 * @see #getLabel()
	 * @generated
	 */
	void setLabel(String value);

	/**
	 * Return the value of the '<em><b>Name Plural</b></em>' attribute
	 * @return the value of the '<em>Name Plural</em>' attribute
	 * @see #setNamePlural(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_NamePlural()
	 * @model
	 * @generated
	 */
	String getNamePlural();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getNamePlural <em>Name Plural</em>}' attribute
	 * @param value the new value of the '<em>Name Plural</em>' attribute
	 * @see #getNamePlural()
	 * @generated
	 */
	void setNamePlural(String value);

	/**
	 * Return the value of the '<em><b>Label Plural</b></em>' attribute
	 * @return the value of the '<em>Label Plural</em>' attribute
	 * @see #setLabelPlural(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_LabelPlural()
	 * @model
	 * @generated
	 */
	String getLabelPlural();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getLabelPlural <em>Label Plural</em>}'
	 * attribute
	 * @param value the new value of the '<em>Label Plural</em>' attribute
	 * @see #getLabelPlural()
	 * @generated
	 */
	void setLabelPlural(String value);

	/**
	 * Return the value of the '<em><b>Discriminator Value</b></em>' attribute
	 * @return the value of the '<em>Discriminator Value</em>' attribute
	 * @see #setDiscriminatorValue(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_DiscriminatorValue()
	 * @model
	 * @generated
	 */
	String getDiscriminatorValue();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorValue <em>Discriminator
	 * Value</em>}' attribute
	 * @param value the new value of the '<em>Discriminator Value</em>' attribute
	 * @see #getDiscriminatorValue()
	 * @generated
	 */
	void setDiscriminatorValue(String value);

	/**
	 * Return the value of the '<em><b>Discriminator Column Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration}.
	 * @return the value of the '<em>Discriminator Column Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration
	 * @see #setDiscriminatorColumnType(DiscriminatorColumnTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_DiscriminatorColumnType()
	 * @model
	 * @generated
	 */
	DiscriminatorColumnTypeEnumeration getDiscriminatorColumnType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorColumnType <em>Discriminator
	 * Column Type</em>}' attribute
	 * @param value the new value of the '<em>Discriminator Column Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration
	 * @see #getDiscriminatorColumnType()
	 * @generated
	 */
	void setDiscriminatorColumnType(DiscriminatorColumnTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Inheritance Type</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration}.
	 * @return the value of the '<em>Inheritance Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration
	 * @see #setInheritanceType(InheritanceTypeEnumeration)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_InheritanceType()
	 * @model
	 * @generated
	 */
	InheritanceTypeEnumeration getInheritanceType();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getInheritanceType <em>Inheritance Type</em>}'
	 * attribute
	 * @param value the new value of the '<em>Inheritance Type</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration
	 * @see #getInheritanceType()
	 * @generated
	 */
	void setInheritanceType(InheritanceTypeEnumeration value);

	/**
	 * Return the value of the '<em><b>Property Access</b></em>' attribute
	 * @return the value of the '<em>Property Access</em>' attribute
	 * @see #setPropertyAccess(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_PropertyAccess()
	 * @model
	 * @generated
	 */
	boolean isPropertyAccess();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#isPropertyAccess <em>Property Access</em>}'
	 * attribute
	 * @param value the new value of the '<em>Property Access</em>' attribute
	 * @see #isPropertyAccess()
	 * @generated
	 */
	void setPropertyAccess(boolean value);

	/**
	 * Return the value of the '<em><b>Abstract</b></em>' attribute
	 * @return the value of the '<em>Abstract</em>' attribute
	 * @see #setAbstract(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Abstract()
	 * @model
	 * @generated
	 */
	boolean isAbstract();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#isAbstract <em>Abstract</em>}' attribute
	 * @param value the new value of the '<em>Abstract</em>' attribute
	 * @see #isAbstract()
	 * @generated
	 */
	void setAbstract(boolean value);

	/**
	 * Return the value of the '<em><b>Mapped Super Class</b></em>' attribute
	 * @return the value of the '<em>Mapped Super Class</em>' attribute
	 * @see #setMappedSuperClass(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_MappedSuperClass()
	 * @model
	 * @generated
	 */
	boolean isMappedSuperClass();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#isMappedSuperClass <em>Mapped Super
	 * Class</em>}' attribute
	 * @param value the new value of the '<em>Mapped Super Class</em>' attribute
	 * @see #isMappedSuperClass()
	 * @generated
	 */
	void setMappedSuperClass(boolean value);

	/**
	 * Return the value of the '<em><b>Parent</b></em>' reference
	 * @return the value of the '<em>Parent</em>' reference
	 * @see #setParent(DomainObject)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Parent()
	 * @model
	 * @generated
	 */
	DomainObject getParent();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getParent <em>Parent</em>}' reference
	 * @param value the new value of the '<em>Parent</em>' reference
	 * @see #getParent()
	 * @generated
	 */
	void setParent(DomainObject value);

	/**
	 * Return the value of the '<em><b>ID Generator</b></em>' containment reference
	 * @return the value of the '<em>ID Generator</em>' containment reference
	 * @see #setIDGenerator(IDGenerator)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_IDGenerator()
	 * @model containment="true"
	 * @generated
	 */
	IDGenerator getIDGenerator();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getIDGenerator <em>ID Generator</em>}'
	 * containment reference
	 * @param value the new value of the '<em>ID Generator</em>' containment reference
	 * @see #getIDGenerator()
	 * @generated
	 */
	void setIDGenerator(IDGenerator value);

	/**
	 * Return the value of the '<em><b>Inheritance</b></em>' containment reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.domain.DomainInheritance#getSource <em>Source</em>}'.
	 * @return the value of the '<em>Inheritance</em>' containment reference
	 * @see #setInheritance(DomainInheritance)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Inheritance()
	 * @see net.codecadenza.eclipse.model.domain.DomainInheritance#getSource
	 * @model opposite="source" containment="true"
	 * @generated
	 */
	DomainInheritance getInheritance();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getInheritance <em>Inheritance</em>}'
	 * containment reference
	 * @param value the new value of the '<em>Inheritance</em>' containment reference
	 * @see #getInheritance()
	 * @generated
	 */
	void setInheritance(DomainInheritance value);

	/**
	 * Return the value of the '<em><b>Attributes</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.domain.DomainAttribute}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainObject <em>Domain Object</em>}'.
	 * @return the value of the '<em>Attributes</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Attributes()
	 * @see net.codecadenza.eclipse.model.domain.DomainAttribute#getDomainObject
	 * @model opposite="domainObject" containment="true"
	 * @generated
	 */
	EList<DomainAttribute> getAttributes();

	/**
	 * Return the value of the '<em><b>Associations</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getDomainObject <em>Domain Object</em>}'.
	 * @return the value of the '<em>Associations</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Associations()
	 * @see net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getDomainObject
	 * @model opposite="domainObject" containment="true"
	 * @generated
	 */
	EList<AbstractDomainAssociation> getAssociations();

	/**
	 * Return the value of the '<em><b>Enum Associations</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.domain.EnumAssociation}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.domain.EnumAssociation#getSource <em>Source</em>}'.
	 * @return the value of the '<em>Enum Associations</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_EnumAssociations()
	 * @see net.codecadenza.eclipse.model.domain.EnumAssociation#getSource
	 * @model opposite="source" containment="true"
	 * @generated
	 */
	EList<EnumAssociation> getEnumAssociations();

	/**
	 * Return the value of the '<em><b>Target Inheritances</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.domain.DomainInheritance}.
	 * @return the value of the '<em>Target Inheritances</em>' reference list
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_TargetInheritances()
	 * @model
	 * @generated
	 */
	EList<DomainInheritance> getTargetInheritances();

	/**
	 * Return the value of the '<em><b>Discriminator Column</b></em>' reference
	 * @return the value of the '<em>Discriminator Column</em>' reference
	 * @see #setDiscriminatorColumn(DBColumn)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_DiscriminatorColumn()
	 * @model
	 * @generated
	 */
	DBColumn getDiscriminatorColumn();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getDiscriminatorColumn <em>Discriminator
	 * Column</em>}' reference
	 * @param value the new value of the '<em>Discriminator Column</em>' reference
	 * @see #getDiscriminatorColumn()
	 * @generated
	 */
	void setDiscriminatorColumn(DBColumn value);

	/**
	 * Return the value of the '<em><b>Database Table</b></em>' containment reference
	 * @return the value of the '<em>Database Table</em>' containment reference
	 * @see #setDatabaseTable(DBTable)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_DatabaseTable()
	 * @model containment="true"
	 * @generated
	 */
	DBTable getDatabaseTable();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getDatabaseTable <em>Database Table</em>}'
	 * containment reference
	 * @param value the new value of the '<em>Database Table</em>' containment reference
	 * @see #getDatabaseTable()
	 * @generated
	 */
	void setDatabaseTable(DBTable value);

	/**
	 * Return the value of the '<em><b>Tag</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.domain.DomainTagEnumeration}.
	 * @return the value of the '<em>Tag</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.DomainTagEnumeration
	 * @see #setTag(DomainTagEnumeration)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainObject_Tag()
	 * @model
	 * @generated
	 */
	DomainTagEnumeration getTag();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainObject#getTag <em>Tag</em>}' attribute
	 * @param value the new value of the '<em>Tag</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.DomainTagEnumeration
	 * @see #getTag()
	 * @generated
	 */
	void setTag(DomainTagEnumeration value);

	/**
	 * Get the full inheritance tree of this domain object
	 * @return a collection of domain objects
	 * @generated not
	 */
	Collection<DomainObject> getFullInheritanceTree();

	/**
	 * @return the attribute that holds the primary key
	 * @generated not
	 */
	DomainAttribute getPKAttribute();

	/**
	 * @return the display attribute
	 * @generated not
	 */
	DomainAttribute getDisplayAttribute();

	/**
	 * @param assocSet
	 * @return a hash set of all valid DTO attributes of this domain object
	 * @generated not
	 */
	HashSet<DomainAttribute> getAllValidDTOAttributes(HashSet<AbstractDomainAssociation> assocSet);

	/**
	 * @return a hash set of all valid DTO attributes of this domain object
	 * @generated not
	 */
	HashSet<DomainAttribute> getAllValidDTOAttributes();

	/**
	 * Get the root domain object of this domain object
	 * @param includeMappedSuperClass
	 * @return the root domain object
	 * @generated not
	 */
	DomainObject getRootParentDomainObject(boolean includeMappedSuperClass);

	/**
	 * @return a list of all valid domain association tags
	 * @param target
	 * @generated not
	 */
	ArrayList<String> getValidAssociationTags(DomainObject target);

	/**
	 * @return a list of all valid domain attribute tags
	 * @generated not
	 */
	ArrayList<String> getValidAttributeTags();

	/**
	 * @return a list with all LOB attributes
	 * @generated not
	 */
	BasicEList<DomainAttribute> getAllLobAttributes();

	/**
	 * @return a list containing all attributes of this domain object including attributes of all inheritance levels
	 * @generated not
	 */
	BasicEList<DomainAttribute> getAllAttributes();

	/**
	 * @return a list containing all associations of this domain object including associations of all inheritance levels
	 * @generated not
	 */
	BasicEList<AbstractDomainAssociation> getAllAssociations();

	/**
	 * @param assocSet
	 * @return true if this domain object is mandated
	 * @generated not
	 */
	boolean isMandated(HashSet<AbstractDomainAssociation> assocSet);

	/**
	 * @return true if this domain object is mandated
	 * @generated not
	 */
	boolean isMandated();

	/**
	 * @param assocSet
	 * @return true if a reference to a domain object exists that represents the user
	 * @generated not
	 */
	boolean hasUserReference(HashSet<AbstractDomainAssociation> assocSet);

	/**
	 * @return true if a reference to a domain object exists that represents the user
	 * @generated not
	 */
	boolean hasUserReference();

	/**
	 * Find a repository method by the given method type and the given domain attribute
	 * @param type
	 * @param attribute
	 * @return the repository method if it exists, or null if it couldn't be found!
	 * @generated not
	 */
	RepositoryMethod findRepositoryMethod(RepositoryMethodTypeEnumeration type, DomainAttribute attribute);

	/**
	 * Find a repository method by the given method type
	 * @param type
	 * @return the repository method if it exists, or null if it couldn't be found!
	 * @generated not
	 */
	RepositoryMethod findRepositoryMethod(RepositoryMethodTypeEnumeration type);

	/**
	 * @return true if sharing of data transfer objects is allowed
	 * @generated not
	 */
	boolean isDTOSharingAllowed();

	/**
	 * @return the shared DTO for this domain object or null if no shared DTO exists
	 * @generated not
	 */
	DTOBean getSharedDTO();

	/**
	 * @return the internal representation of the domain object source file
	 * @generated not
	 */
	JavaFile getSourceFile();

	/**
	 * @return the internal representation of the domain object's callback listener source file
	 * @generated not
	 */
	JavaFile getListenerSourceFile();

	/**
	 * @return the internal representation of the domain object's meta-model source file
	 * @generated not
	 */
	JavaFile getMetaModelSourceFile();

}
