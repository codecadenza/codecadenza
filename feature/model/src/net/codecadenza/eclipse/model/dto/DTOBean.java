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
package net.codecadenza.eclipse.model.dto;

import java.util.List;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.mapping.MappingObject;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>DTO Bean</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.dto.DTOBean#getAttributes <em>Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.DTOBean#isStandardConversion <em>Standard Conversion</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.DTOBean#isShared <em>Shared</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.DTOBean#isCreatedManually <em>Created Manually</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBean()
 * @model
 * @generated
 */
public interface DTOBean extends MappingObject {
	/**
	 * Return the value of the '<em><b>Attributes</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getDTOBean <em>DTO Bean</em>}'.
	 * @return the value of the '<em>Attributes</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBean_Attributes()
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getDTOBean
	 * @model opposite="dTOBean" containment="true"
	 * @generated
	 */
	EList<DTOBeanAttribute> getAttributes();

	/**
	 * Return the value of the '<em><b>Standard Conversion</b></em>' attribute
	 * @return the value of the '<em>Standard Conversion</em>' attribute
	 * @see #setStandardConversion(boolean)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBean_StandardConversion()
	 * @model
	 * @generated
	 */
	boolean isStandardConversion();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.dto.DTOBean#isStandardConversion <em>Standard Conversion</em>}'
	 * attribute
	 * @param value the new value of the '<em>Standard Conversion</em>' attribute
	 * @see #isStandardConversion()
	 * @generated
	 */
	void setStandardConversion(boolean value);

	/**
	 * Return the value of the '<em><b>Shared</b></em>' attribute
	 * @return the value of the '<em>Shared</em>' attribute
	 * @see #setShared(boolean)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBean_Shared()
	 * @model
	 * @generated
	 */
	boolean isShared();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.dto.DTOBean#isShared <em>Shared</em>}' attribute
	 * @param value the new value of the '<em>Shared</em>' attribute
	 * @see #isShared()
	 * @generated
	 */
	void setShared(boolean value);

	/**
	 * Return the value of the '<em><b>Created Manually</b></em>' attribute
	 * @return the value of the '<em>Created Manually</em>' attribute
	 * @see #setCreatedManually(boolean)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBean_CreatedManually()
	 * @model
	 * @generated
	 */
	boolean isCreatedManually();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.dto.DTOBean#isCreatedManually <em>Created Manually</em>}'
	 * attribute
	 * @param value the new value of the '<em>Created Manually</em>' attribute
	 * @see #isCreatedManually()
	 * @generated
	 */
	void setCreatedManually(boolean value);

	/**
	 * @return the attribute that represents the primary key
	 * @generated not
	 */
	DTOBeanAttribute getPKAttribute();

	/**
	 * @return the attribute that represents the display attribute, or null if no display attribute exists!
	 * @generated not
	 */
	DTOBeanAttribute getDisplayAttribute();

	/**
	 * @return the name of the class
	 * @generated not
	 */
	String getModelClassName();

	/**
	 * @return true if this DTO is relevant for internal meta-model only
	 * @generated not
	 */
	boolean isVirtual();

	/**
	 * @param domainAttribute
	 * @param assoc
	 * @param assocList
	 * @return the DTO attribute that matches the filter criteria, or null if it could not be found
	 * @generated not
	 */
	DTOBeanAttribute searchAttribute(DomainAttribute domainAttribute, AbstractDomainAssociation assoc,
			List<AbstractDomainAssociation> assocList);

	/**
	 * @param listDTO
	 * @param assoc
	 * @param assocList
	 * @return the DTO attribute that matches the filter criteria, or null if it could not be found
	 * @generated not
	 */
	DTOBeanAttribute searchAttribute(DTOBean listDTO, AbstractDomainAssociation assoc, List<AbstractDomainAssociation> assocList);

	/**
	 * @return true if the generator should add JAXB annotations
	 * @generated not
	 */
	boolean addJAXBAnnotations();

	/**
	 * @return true if this data transfer object is used by an integration method
	 * @generated not
	 */
	boolean isUsedByIntegrationMethod();

	/**
	 * Add a new DTO attribute based on the given domain attribute
	 * @param domainAttribute
	 * @param useExisting if true the method searches for an existing attribute with the same mapping. If an existing attribute has
	 *          been found it will be returned!
	 * @return either a new or an existing DTO attribute
	 * @throws IllegalStateException when creating a new DTO attribute that has the same name as an existing attribute
	 * @generated not
	 */
	DTOBeanAttribute addAttribute(DomainAttribute domainAttribute, boolean useExisting);

	/**
	 * Add a new DTO attribute based on the given domain attribute
	 * @param domainAttribute
	 * @param assocList
	 * @param useExisting if true the method searches for an existing attribute with the same mapping. If an existing attribute has
	 *          been found it will be returned!
	 * @return either a new or an existing DTO attribute
	 * @throws IllegalStateException when creating a new DTO attribute that has the same name as an existing attribute
	 * @generated not
	 */
	DTOBeanAttribute addAttribute(DomainAttribute domainAttribute, List<AbstractDomainAssociation> assocList, boolean useExisting);

	/**
	 * Add a new DTO attribute based on the given domain attribute
	 * @param domainAttribute
	 * @param name the name of the new attribute. If this parameter is null or empty a default name will be generated!
	 * @param assocList
	 * @param useExisting if true the method searches for an existing attribute with the same mapping. If an existing attribute has
	 *          been found it will be returned!
	 * @return either a new or an existing DTO attribute
	 * @throws IllegalStateException when creating a new DTO attribute that has the same name as an existing attribute
	 * @generated not
	 */
	DTOBeanAttribute addAttribute(DomainAttribute domainAttribute, String name, List<AbstractDomainAssociation> assocList,
			boolean useExisting);

	/**
	 * Add a new DTO attribute based on the given domain attribute
	 * @param domainAttribute
	 * @param assoc
	 * @param useExisting if true the method searches for an existing attribute with the same mapping. If an existing attribute has
	 *          been found it will be returned!
	 * @return either a new or an existing DTO attribute
	 * @throws IllegalStateException when creating a new DTO attribute that has the same name as an existing attribute
	 * @generated not
	 */
	DTOBeanAttribute addAttribute(DomainAttribute domainAttribute, AbstractDomainAssociation assoc, boolean useExisting);

	/**
	 * Add a new DTO attribute based on the given domain association provided by the association list
	 * @param referencedDTO
	 * @param name the name of the new attribute. If this parameter is null or empty a default name will be generated!
	 * @param assocList
	 * @param useExisting if true the method searches for an existing attribute with the same mapping. If an existing attribute has
	 *          been found it will be returned!
	 * @return either a new or an existing DTO attribute
	 * @throws IllegalStateException when creating a new DTO attribute that has the same name as an existing attribute
	 * @generated not
	 */
	DTOBeanAttribute addAttribute(DTOBean referencedDTO, String name, List<AbstractDomainAssociation> assocList,
			boolean useExisting);

	/**
	 * Add a new DTO attribute based on the given domain association provided by the association list
	 * @param referencedDTO
	 * @param assocList
	 * @param useExisting if true the method searches for an existing attribute with the same mapping. If an existing attribute has
	 *          been found it will be returned!
	 * @return either a new or an existing DTO attribute
	 * @throws IllegalStateException when creating a new DTO attribute that has the same name as an existing attribute
	 * @generated not
	 */
	DTOBeanAttribute addAttribute(DTOBean referencedDTO, List<AbstractDomainAssociation> assocList, boolean useExisting);

	/**
	 * Add a new DTO attribute based on the given domain association
	 * @param referencedDTO
	 * @param assoc
	 * @param useExisting if true the method searches for an existing attribute with the same mapping. If an existing attribute has
	 *          been found it will be returned!
	 * @return either a new or an existing DTO attribute
	 * @throws IllegalStateException when creating a new DTO attribute that has the same name as an existing attribute
	 * @generated not
	 */
	DTOBeanAttribute addAttribute(DTOBean referencedDTO, AbstractDomainAssociation assoc, boolean useExisting);

	/**
	 * @return the attribute that represents the primary key of the associated client
	 * @throws IllegalStateException if the respective attribute could not be found
	 * @generated not
	 */
	DTOBeanAttribute getClientPKAttribute();

	/**
	 * @return the internal representation of the source file for an Angular application
	 * @generated not
	 */
	WorkspaceFile getTypeScriptSourceFile();
}
