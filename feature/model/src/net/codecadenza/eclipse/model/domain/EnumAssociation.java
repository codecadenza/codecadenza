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

import net.codecadenza.eclipse.model.java.JavaEnum;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Enum Association</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.EnumAssociation#getSource <em>Source</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.EnumAssociation#getTarget <em>Target</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getEnumAssociation()
 * @model
 * @generated
 */
public interface EnumAssociation extends EObject {
	/**
	 * Return the value of the '<em><b>Source</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.domain.DomainObject#getEnumAssociations <em>Enum Associations</em>}'.
	 * @return the value of the '<em>Source</em>' container reference
	 * @see #setSource(DomainObject)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getEnumAssociation_Source()
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getEnumAssociations
	 * @model opposite="enumAssociations"
	 * @generated
	 */
	DomainObject getSource();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.EnumAssociation#getSource <em>Source</em>}' container
	 * reference
	 * @param value the new value of the '<em>Source</em>' container reference
	 * @see #getSource()
	 * @generated
	 */
	void setSource(DomainObject value);

	/**
	 * Return the value of the '<em><b>Target</b></em>' reference
	 * @return the value of the '<em>Target</em>' reference
	 * @see #setTarget(JavaEnum)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getEnumAssociation_Target()
	 * @model
	 * @generated
	 */
	JavaEnum getTarget();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.EnumAssociation#getTarget <em>Target</em>}' reference
	 * @param value the new value of the '<em>Target</em>' reference
	 * @see #getTarget()
	 * @generated
	 */
	void setTarget(JavaEnum value);

	/**
	 * Return the value of the '<em><b>Domain Attribute</b></em>' reference
	 * @return the value of the '<em>Domain Attribute</em>' reference
	 * @see #setDomainAttribute(DomainAttribute)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getEnumAssociation_DomainAttribute()
	 * @model
	 * @generated
	 */
	DomainAttribute getDomainAttribute();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.EnumAssociation#getDomainAttribute <em>Domain
	 * Attribute</em>}' reference
	 * @param value the new value of the '<em>Domain Attribute</em>' reference
	 * @see #getDomainAttribute()
	 * @generated
	 */
	void setDomainAttribute(DomainAttribute value);

}
