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

import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Abstract Domain Association</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getName <em>Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isOwner <em>Owner</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadePersist <em>Cascade Persist</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeMerge <em>Cascade Merge</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeRemove <em>Cascade Remove</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeRefresh <em>Cascade Refresh</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isFetchTypeEager <em>Fetch Type Eager</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getDomainObject <em>Domain Object</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getTarget <em>Target</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getTag <em>Tag</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getInternalComment <em>Internal Comment</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getUserComment <em>User Comment</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation()
 * @model abstract="true"
 * @generated
 */
public interface AbstractDomainAssociation extends EObject {
	/**
	 * Return the value of the '<em><b>Name</b></em>' attribute
	 * @return the value of the '<em>Name</em>' attribute
	 * @see #setName(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_Name()
	 * @model
	 * @generated
	 */
	String getName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getName <em>Name</em>}' attribute
	 * @param value the new value of the '<em>Name</em>' attribute
	 * @see #getName()
	 * @generated
	 */
	void setName(String value);

	/**
	 * Return the value of the '<em><b>Owner</b></em>' attribute
	 * @return the value of the '<em>Owner</em>' attribute
	 * @see #setOwner(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_Owner()
	 * @model
	 * @generated
	 */
	boolean isOwner();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isOwner <em>Owner</em>}'
	 * attribute
	 * @param value the new value of the '<em>Owner</em>' attribute
	 * @see #isOwner()
	 * @generated
	 */
	void setOwner(boolean value);

	/**
	 * Return the value of the '<em><b>Cascade Persist</b></em>' attribute
	 * @return the value of the '<em>Cascade Persist</em>' attribute
	 * @see #setCascadePersist(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_CascadePersist()
	 * @model
	 * @generated
	 */
	boolean isCascadePersist();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadePersist <em>Cascade
	 * Persist</em>}' attribute
	 * @param value the new value of the '<em>Cascade Persist</em>' attribute
	 * @see #isCascadePersist()
	 * @generated
	 */
	void setCascadePersist(boolean value);

	/**
	 * Return the value of the '<em><b>Cascade Merge</b></em>' attribute
	 * @return the value of the '<em>Cascade Merge</em>' attribute
	 * @see #setCascadeMerge(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_CascadeMerge()
	 * @model
	 * @generated
	 */
	boolean isCascadeMerge();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeMerge <em>Cascade
	 * Merge</em>}' attribute
	 * @param value the new value of the '<em>Cascade Merge</em>' attribute
	 * @see #isCascadeMerge()
	 * @generated
	 */
	void setCascadeMerge(boolean value);

	/**
	 * Return the value of the '<em><b>Cascade Remove</b></em>' attribute
	 * @return the value of the '<em>Cascade Remove</em>' attribute
	 * @see #setCascadeRemove(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_CascadeRemove()
	 * @model
	 * @generated
	 */
	boolean isCascadeRemove();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeRemove <em>Cascade
	 * Remove</em>}' attribute
	 * @param value the new value of the '<em>Cascade Remove</em>' attribute
	 * @see #isCascadeRemove()
	 * @generated
	 */
	void setCascadeRemove(boolean value);

	/**
	 * Return the value of the '<em><b>Cascade Refresh</b></em>' attribute
	 * @return the value of the '<em>Cascade Refresh</em>' attribute
	 * @see #setCascadeRefresh(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_CascadeRefresh()
	 * @model
	 * @generated
	 */
	boolean isCascadeRefresh();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isCascadeRefresh <em>Cascade
	 * Refresh</em>}' attribute
	 * @param value the new value of the '<em>Cascade Refresh</em>' attribute
	 * @see #isCascadeRefresh()
	 * @generated
	 */
	void setCascadeRefresh(boolean value);

	/**
	 * Return the value of the '<em><b>Fetch Type Eager</b></em>' attribute
	 * @return the value of the '<em>Fetch Type Eager</em>' attribute
	 * @see #setFetchTypeEager(boolean)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_FetchTypeEager()
	 * @model
	 * @generated
	 */
	boolean isFetchTypeEager();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#isFetchTypeEager <em>Fetch Type
	 * Eager</em>}' attribute
	 * @param value the new value of the '<em>Fetch Type Eager</em>' attribute
	 * @see #isFetchTypeEager()
	 * @generated
	 */
	void setFetchTypeEager(boolean value);

	/**
	 * Return the value of the '<em><b>Domain Object</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.domain.DomainObject#getAssociations <em>Associations</em>}'.
	 * @return the value of the '<em>Domain Object</em>' container reference
	 * @see #setDomainObject(DomainObject)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_DomainObject()
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getAssociations
	 * @model opposite="associations"
	 * @generated
	 */
	DomainObject getDomainObject();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getDomainObject <em>Domain
	 * Object</em>}' container reference
	 * @param value the new value of the '<em>Domain Object</em>' container reference
	 * @see #getDomainObject()
	 * @generated
	 */
	void setDomainObject(DomainObject value);

	/**
	 * Return the value of the '<em><b>Target</b></em>' reference
	 * @return the value of the '<em>Target</em>' reference
	 * @see #setTarget(DomainObject)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_Target()
	 * @model
	 * @generated
	 */
	DomainObject getTarget();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getTarget <em>Target</em>}'
	 * reference
	 * @param value the new value of the '<em>Target</em>' reference
	 * @see #getTarget()
	 * @generated
	 */
	void setTarget(DomainObject value);

	/**
	 * Return the value of the '<em><b>Tag</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.domain.AssociationTagEnumeration}.
	 * @return the value of the '<em>Tag</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.AssociationTagEnumeration
	 * @see #setTag(AssociationTagEnumeration)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_Tag()
	 * @model
	 * @generated
	 */
	AssociationTagEnumeration getTag();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getTag <em>Tag</em>}' attribute
	 * @param value the new value of the '<em>Tag</em>' attribute
	 * @see net.codecadenza.eclipse.model.domain.AssociationTagEnumeration
	 * @see #getTag()
	 * @generated
	 */
	void setTag(AssociationTagEnumeration value);

	/**
	 * Return the value of the '<em><b>Internal Comment</b></em>' attribute
	 * @return the value of the '<em>Internal Comment</em>' attribute
	 * @see #setInternalComment(String)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_InternalComment()
	 * @model
	 * @generated
	 */
	String getInternalComment();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getInternalComment <em>Internal
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
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_UserComment()
	 * @model
	 * @generated
	 */
	String getUserComment();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getUserComment <em>User
	 * Comment</em>}' attribute
	 * @param value the new value of the '<em>User Comment</em>' attribute
	 * @see #getUserComment()
	 * @generated
	 */
	void setUserComment(String value);

	/**
	 * Return the value of the '<em><b>Reverse Association</b></em>' reference
	 * @return the value of the '<em>Reverse Association</em>' reference
	 * @see #setReverseAssociation(AbstractDomainAssociation)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getAbstractDomainAssociation_ReverseAssociation()
	 * @model
	 * @generated
	 */
	AbstractDomainAssociation getReverseAssociation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.AbstractDomainAssociation#getReverseAssociation <em>Reverse
	 * Association</em>}' reference
	 * @param value the new value of the '<em>Reverse Association</em>' reference
	 * @see #getReverseAssociation()
	 * @generated
	 */
	void setReverseAssociation(AbstractDomainAssociation value);

	/**
	 * @return the upper-case name of this association (e.g. Value)
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
	 * @return the method reference of the getter (e.g. ::getValue)
	 * @generated not
	 */
	String getGetterReference();

	/**
	 * @return the method reference of the setter (e.g. ::setValue)
	 * @generated not
	 */
	String getSetterReference();

}
