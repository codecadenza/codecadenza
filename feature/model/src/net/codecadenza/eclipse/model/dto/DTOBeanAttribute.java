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

import net.codecadenza.eclipse.model.mapping.MappingAttribute;

/**
 * A representation of the model object '<em><b>DTO Bean Attribute</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getDTOBean <em>DTO Bean</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getReferencedDTOBean <em>Referenced DTO Bean</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getSelectToken <em>Select Token</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#isLovReturn <em>Lov Return</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBeanAttribute()
 * @model
 * @generated
 */
public interface DTOBeanAttribute extends MappingAttribute {
	/**
	 * Return the value of the '<em><b>DTO Bean</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.dto.DTOBean#getAttributes <em>Attributes</em>}'.
	 * @return the value of the '<em>DTO Bean</em>' container reference
	 * @see #setDTOBean(DTOBean)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBeanAttribute_DTOBean()
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#getAttributes
	 * @model opposite="attributes"
	 * @generated
	 */
	DTOBean getDTOBean();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getDTOBean <em>DTO Bean</em>}' container
	 * reference
	 * @param value the new value of the '<em>DTO Bean</em>' container reference
	 * @see #getDTOBean()
	 * @generated
	 */
	void setDTOBean(DTOBean value);

	/**
	 * Return the value of the '<em><b>Referenced DTO Bean</b></em>' reference
	 * @return the value of the '<em>Referenced DTO Bean</em>' reference
	 * @see #setReferencedDTOBean(DTOBean)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBeanAttribute_ReferencedDTOBean()
	 * @model
	 * @generated
	 */
	DTOBean getReferencedDTOBean();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getReferencedDTOBean <em>Referenced DTO
	 * Bean</em>}' reference
	 * @param value the new value of the '<em>Referenced DTO Bean</em>' reference
	 * @see #getReferencedDTOBean()
	 * @generated
	 */
	void setReferencedDTOBean(DTOBean value);

	/**
	 * Return the value of the '<em><b>Select Token</b></em>' attribute
	 * @return the value of the '<em>Select Token</em>' attribute
	 * @see #setSelectToken(String)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBeanAttribute_SelectToken()
	 * @model
	 * @generated
	 */
	String getSelectToken();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getSelectToken <em>Select Token</em>}'
	 * attribute
	 * @param value the new value of the '<em>Select Token</em>' attribute
	 * @see #getSelectToken()
	 * @generated
	 */
	void setSelectToken(String value);

	/**
	 * Return the value of the '<em><b>Lov Return</b></em>' attribute
	 * @return the value of the '<em>Lov Return</em>' attribute
	 * @see #setLovReturn(boolean)
	 * @see net.codecadenza.eclipse.model.dto.DtoPackage#getDTOBeanAttribute_LovReturn()
	 * @model
	 * @generated
	 */
	boolean isLovReturn();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.dto.DTOBeanAttribute#isLovReturn <em>Lov Return</em>}' attribute
	 * @param value the new value of the '<em>Lov Return</em>' attribute
	 * @see #isLovReturn()
	 * @generated
	 */
	void setLovReturn(boolean value);

	/**
	 * @return the name of the getter (e.g. getValue())
	 * @generated not
	 */
	String getModelGetterName();

	/**
	 * @return the name of the field
	 * @generated not
	 */
	String getModelFieldName();

	/**
	 * @return the name of the setter
	 * @generated not
	 */
	String getModelSetterName();

	/**
	 * @return the name of the constant for the attributes's name (e.g. ATTR_NAME)
	 * @generated not
	 */
	String getAttributeNameConstantName();

	/**
	 * @return the constant for the attributes's name (e.g. CustomerDTO.ATTR_NAME)
	 * @generated not
	 */
	String getAttributeNameConstant();

	/**
	 * @return the name of the constant for the select token (e.g. SELECT_NAME)
	 * @generated not
	 */
	String getSelectTokenConstantName();

	/**
	 * @return the constant for the select token (e.g. CustomerDTO.SELECT_NAME)
	 * @generated not
	 */
	String getSelectTokenConstant();

}
