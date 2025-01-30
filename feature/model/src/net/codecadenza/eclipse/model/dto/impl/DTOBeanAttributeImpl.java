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
package net.codecadenza.eclipse.model.dto.impl;

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.dto.DtoPackage;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.util.JavaBeanHelper;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EcoreUtil;

/**
 * An implementation of the model object '<em><b>DTO Bean Attribute</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanAttributeImpl#getDTOBean <em>DTO Bean</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanAttributeImpl#getReferencedDTOBean <em>Referenced DTO Bean</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanAttributeImpl#getSelectToken <em>Select Token</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanAttributeImpl#isLovReturn <em>Lov Return</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DTOBeanAttributeImpl extends MappingAttributeImpl implements DTOBeanAttribute {
	/**
	 * The cached value of the '{@link #getReferencedDTOBean() <em>Referenced DTO Bean</em>}' reference
	 * @see #getReferencedDTOBean()
	 * @generated
	 * @ordered
	 */
	protected DTOBean referencedDTOBean;

	/**
	 * The default value of the '{@link #getSelectToken() <em>Select Token</em>}' attribute
	 * @see #getSelectToken()
	 * @generated
	 * @ordered
	 */
	protected static final String SELECT_TOKEN_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSelectToken() <em>Select Token</em>}' attribute
	 * @see #getSelectToken()
	 * @generated
	 * @ordered
	 */
	protected String selectToken = SELECT_TOKEN_EDEFAULT;

	/**
	 * The default value of the '{@link #isLovReturn() <em>Lov Return</em>}' attribute
	 * @see #isLovReturn()
	 * @generated
	 * @ordered
	 */
	protected static final boolean LOV_RETURN_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isLovReturn() <em>Lov Return</em>}' attribute
	 * @see #isLovReturn()
	 * @generated
	 * @ordered
	 */
	protected boolean lovReturn = LOV_RETURN_EDEFAULT;

	/**
	 * @generated
	 */
	protected DTOBeanAttributeImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DtoPackage.Literals.DTO_BEAN_ATTRIBUTE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getDTOBean()
	 * @generated
	 */
	@Override
	public DTOBean getDTOBean() {
		if (eContainerFeatureID() != DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN)
			return null;

		return (DTOBean) eInternalContainer();
	}

	/**
	 * @param newDTOBean
	 * @param msgs
	 * @return a chain of notifications
	 * @generated
	 */
	public NotificationChain basicSetDTOBean(DTOBean newDTOBean, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject) newDTOBean, DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN, msgs);
		return msgs;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#setDTOBean(net.codecadenza.eclipse.model.dto.DTOBean)
	 * @generated
	 */
	@Override
	public void setDTOBean(DTOBean newDTOBean) {
		if (newDTOBean != eInternalContainer()
				|| (eContainerFeatureID() != DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN && newDTOBean != null)) {
			if (EcoreUtil.isAncestor(this, newDTOBean))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());

			NotificationChain msgs = null;

			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);

			if (newDTOBean != null)
				msgs = ((InternalEObject) newDTOBean).eInverseAdd(this, DtoPackage.DTO_BEAN__ATTRIBUTES, DTOBean.class, msgs);
			msgs = basicSetDTOBean(newDTOBean, msgs);

			if (msgs != null)
				msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN, newDTOBean, newDTOBean));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getReferencedDTOBean()
	 * @generated
	 */
	@Override
	public DTOBean getReferencedDTOBean() {
		if (referencedDTOBean != null && referencedDTOBean.eIsProxy()) {
			final var oldReferencedDTOBean = (InternalEObject) referencedDTOBean;
			referencedDTOBean = (DTOBean) eResolveProxy(oldReferencedDTOBean);

			if (referencedDTOBean != oldReferencedDTOBean && eNotificationRequired())
				eNotify(new ENotificationImpl(this, Notification.RESOLVE, DtoPackage.DTO_BEAN_ATTRIBUTE__REFERENCED_DTO_BEAN,
						oldReferencedDTOBean, referencedDTOBean));
		}

		return referencedDTOBean;
	}

	/**
	 * @return the cached object instance
	 * @generated
	 */
	public DTOBean basicGetReferencedDTOBean() {
		return referencedDTOBean;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#setReferencedDTOBean(net.codecadenza.eclipse.model.dto.DTOBean)
	 * @generated
	 */
	@Override
	public void setReferencedDTOBean(DTOBean newReferencedDTOBean) {
		final DTOBean oldReferencedDTOBean = referencedDTOBean;
		referencedDTOBean = newReferencedDTOBean;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DtoPackage.DTO_BEAN_ATTRIBUTE__REFERENCED_DTO_BEAN,
					oldReferencedDTOBean, referencedDTOBean));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getSelectToken()
	 * @generated
	 */
	@Override
	public String getSelectToken() {
		return selectToken;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#setSelectToken(java.lang.String)
	 * @generated
	 */
	@Override
	public void setSelectToken(String newSelectToken) {
		final String oldSelectToken = selectToken;
		selectToken = newSelectToken;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DtoPackage.DTO_BEAN_ATTRIBUTE__SELECT_TOKEN, oldSelectToken,
					selectToken));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#isLovReturn()
	 * @generated
	 */
	@Override
	public boolean isLovReturn() {
		return lovReturn;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#setLovReturn(boolean)
	 * @generated
	 */
	@Override
	public void setLovReturn(boolean newLovReturn) {
		final boolean oldLovReturn = lovReturn;
		lovReturn = newLovReturn;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DtoPackage.DTO_BEAN_ATTRIBUTE__LOV_RETURN, oldLovReturn, lovReturn));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);

				return basicSetDTOBean((DTOBean) otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN:
				return basicSetDTOBean(null, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#eBasicRemoveFromContainerFeature(org.eclipse.emf.common.notify.
	 * NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
		switch (eContainerFeatureID()) {
			case DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN:
				return eInternalContainer().eInverseRemove(this, DtoPackage.DTO_BEAN__ATTRIBUTES, DTOBean.class, msgs);
		}

		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN:
				return getDTOBean();
			case DtoPackage.DTO_BEAN_ATTRIBUTE__REFERENCED_DTO_BEAN:
				if (resolve)
					return getReferencedDTOBean();

				return basicGetReferencedDTOBean();
			case DtoPackage.DTO_BEAN_ATTRIBUTE__SELECT_TOKEN:
				return getSelectToken();
			case DtoPackage.DTO_BEAN_ATTRIBUTE__LOV_RETURN:
				return isLovReturn();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN:
				setDTOBean((DTOBean) newValue);
				return;
			case DtoPackage.DTO_BEAN_ATTRIBUTE__REFERENCED_DTO_BEAN:
				setReferencedDTOBean((DTOBean) newValue);
				return;
			case DtoPackage.DTO_BEAN_ATTRIBUTE__SELECT_TOKEN:
				setSelectToken((String) newValue);
				return;
			case DtoPackage.DTO_BEAN_ATTRIBUTE__LOV_RETURN:
				setLovReturn((Boolean) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN:
				setDTOBean((DTOBean) null);
				return;
			case DtoPackage.DTO_BEAN_ATTRIBUTE__REFERENCED_DTO_BEAN:
				setReferencedDTOBean((DTOBean) null);
				return;
			case DtoPackage.DTO_BEAN_ATTRIBUTE__SELECT_TOKEN:
				setSelectToken(SELECT_TOKEN_EDEFAULT);
				return;
			case DtoPackage.DTO_BEAN_ATTRIBUTE__LOV_RETURN:
				setLovReturn(LOV_RETURN_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN:
				return getDTOBean() != null;
			case DtoPackage.DTO_BEAN_ATTRIBUTE__REFERENCED_DTO_BEAN:
				return referencedDTOBean != null;
			case DtoPackage.DTO_BEAN_ATTRIBUTE__SELECT_TOKEN:
				return selectToken != null;
			case DtoPackage.DTO_BEAN_ATTRIBUTE__LOV_RETURN:
				return lovReturn != LOV_RETURN_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (selectToken: ");
		result.append(selectToken);
		result.append(", lovReturn: ");
		result.append(lovReturn);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getModelGetterName()
	 * @generated not
	 */
	@Override
	public String getModelGetterName() {
		if (getDTOBean().getNamespace().getProject().isBoundaryMode())
			return getGetterName();

		final DomainAttribute domainAttribute = getDomainAttribute();

		if (domainAttribute != null) {
			if (getAssociation() == null)
				return domainAttribute.getGetterName();

			final String getter = getAssociation().getGetterName() + "." + domainAttribute.getGetterName();

			return getAssociationListGetter() + getter;
		}

		final String getter = getAssociation().getGetterName();

		return getAssociationListGetter() + getter;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getModelFieldName()
	 * @generated not
	 */
	@Override
	public String getModelFieldName() {
		final DomainAttribute domainAttribute = getDomainAttribute();

		if (getDTOBean().getNamespace().getProject().isBoundaryMode())
			return JavaBeanHelper.getPropertyName(getName());

		if (domainAttribute != null) {
			if (getAssociation() == null)
				return JavaBeanHelper.getPropertyName(domainAttribute.getName());

			var fieldName = JavaBeanHelper.getPropertyName(getAssociation().getName()) + ".";
			fieldName += JavaBeanHelper.getPropertyName(domainAttribute.getName());

			if (!getAssociationList().isEmpty()) {
				final var s = new StringBuilder();

				for (int i = getAssociationList().size() - 1; i >= 0; i--) {
					final AbstractDomainAssociation ia = getAssociationList().get(i);
					s.append(JavaBeanHelper.getPropertyName(ia.getName()) + ".");
				}

				fieldName = s.toString() + fieldName;
			}

			return fieldName;
		}

		final String fieldName = JavaBeanHelper.getPropertyName(getAssociation().getName());
		final var s = new StringBuilder();

		if (!getAssociationList().isEmpty())
			for (int i = getAssociationList().size() - 1; i >= 0; i--) {
				final AbstractDomainAssociation ia = getAssociationList().get(i);
				s.append(JavaBeanHelper.getPropertyName(ia.getName()) + ".");
			}

		return s.toString() + fieldName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getModelSetterName()
	 * @generated not
	 */
	@Override
	public String getModelSetterName() {
		final DomainAttribute domainAttribute = getDomainAttribute();

		if (getDTOBean().getNamespace().getProject().isBoundaryMode())
			return getSetterName();

		if (domainAttribute != null) {
			if (getAssociation() == null)
				return domainAttribute.getSetterName();

			final String setter = getAssociation().getGetterName() + "." + domainAttribute.getSetterName();

			return getAssociationListGetter() + setter;
		}

		final String setter = getAssociation().getSetterName();

		return getAssociationListGetter() + setter;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getAttributeNameConstantName()
	 * @generated not
	 */
	@Override
	public String getAttributeNameConstantName() {
		return "ATTR_" + getName().toUpperCase();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getAttributeNameConstant()
	 * @generated not
	 */
	@Override
	public String getAttributeNameConstant() {
		return getDTOBean().getName() + "." + getAttributeNameConstantName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getSelectTokenConstantName()
	 * @generated not
	 */
	@Override
	public String getSelectTokenConstantName() {
		return "SELECT_" + getName().toUpperCase();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBeanAttribute#getSelectTokenConstant()
	 * @generated not
	 */
	@Override
	public String getSelectTokenConstant() {
		return getDTOBean().getName() + "." + getSelectTokenConstantName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingAttributeImpl#getSearchType()
	 * @generated not
	 */
	@Override
	public JavaType getSearchType() {
		super.getSearchType();

		// There is a possibility that a wrapper type is returned that is not required in the corresponding context, since a
		// detailed analysis of the corresponding query is omitted here!
		if (getDTOBean().isStandardConversion() || getAssociation() == null || !getDomainAttribute().getJavaType().isPrimitive())
			return getDomainAttribute().getJavaType();

		final Project project = getDTOBean().getNamespace().getProject();

		if ((getAssociation() instanceof final ManyToOneAssociation mto && mto.isOptional())
				|| (getAssociation() instanceof final OneToOneAssociation oto && oto.isOptional()))
			return project.getJavaTypeByName(getDomainAttribute().getJavaType().getWrapperTypeName());

		for (final AbstractDomainAssociation assoc : getAssociationList()) {
			// Always use a wrapper type if the attribute is mapped over either a many-to-many or one-to-many association as there
			// could be a left outer join!
			if ((assoc instanceof final ManyToOneAssociation mto && mto.isOptional())
					|| (assoc instanceof final OneToOneAssociation oto && oto.isOptional())
					|| (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation))
				return project.getJavaTypeByName(getDomainAttribute().getJavaType().getWrapperTypeName());
		}

		return getDomainAttribute().getJavaType();
	}

}
