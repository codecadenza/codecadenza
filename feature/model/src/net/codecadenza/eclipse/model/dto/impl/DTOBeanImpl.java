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

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_DOMAIN_FOLDER;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.dto.DtoFactory;
import net.codecadenza.eclipse.model.dto.DtoPackage;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.IntegrationTechnology;
import net.codecadenza.eclipse.model.project.MappingAnnotationStrategy;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.ENotificationImpl;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.InternalEList;

/**
 * An implementation of the model object '<em><b>DTO Bean</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanImpl#getAttributes <em>Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanImpl#isStandardConversion <em>Standard Conversion</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanImpl#isShared <em>Shared</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.dto.impl.DTOBeanImpl#isCreatedManually <em>Created Manually</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class DTOBeanImpl extends MappingObjectImpl implements DTOBean {
	/**
	 * The cached value of the '{@link #getAttributes() <em>Attributes</em>}' containment reference list
	 * @see #getAttributes()
	 * @generated
	 * @ordered
	 */
	protected EList<DTOBeanAttribute> attributes;

	/**
	 * The default value of the '{@link #isStandardConversion() <em>Standard Conversion</em>}' attribute
	 * @see #isStandardConversion()
	 * @generated
	 * @ordered
	 */
	protected static final boolean STANDARD_CONVERSION_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isStandardConversion() <em>Standard Conversion</em>}' attribute
	 * @see #isStandardConversion()
	 * @generated
	 * @ordered
	 */
	protected boolean standardConversion = STANDARD_CONVERSION_EDEFAULT;

	/**
	 * The default value of the '{@link #isShared() <em>Shared</em>}' attribute
	 * @see #isShared()
	 * @generated
	 * @ordered
	 */
	protected static final boolean SHARED_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isShared() <em>Shared</em>}' attribute
	 * @see #isShared()
	 * @generated
	 * @ordered
	 */
	protected boolean shared = SHARED_EDEFAULT;

	/**
	 * The default value of the '{@link #isCreatedManually() <em>Created Manually</em>}' attribute
	 * @see #isCreatedManually()
	 * @generated
	 * @ordered
	 */
	protected static final boolean CREATED_MANUALLY_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isCreatedManually() <em>Created Manually</em>}' attribute
	 * @see #isCreatedManually()
	 * @generated
	 * @ordered
	 */
	protected boolean createdManually = CREATED_MANUALLY_EDEFAULT;

	/**
	 * @generated
	 */
	protected DTOBeanImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return DtoPackage.Literals.DTO_BEAN;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#getAttributes()
	 * @generated
	 */
	@Override
	public EList<DTOBeanAttribute> getAttributes() {
		if (attributes == null)
			attributes = new EObjectContainmentWithInverseEList<>(DTOBeanAttribute.class, this, DtoPackage.DTO_BEAN__ATTRIBUTES,
					DtoPackage.DTO_BEAN_ATTRIBUTE__DTO_BEAN);

		return attributes;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#isStandardConversion()
	 * @generated
	 */
	@Override
	public boolean isStandardConversion() {
		return standardConversion;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#setStandardConversion(boolean)
	 * @generated
	 */
	@Override
	public void setStandardConversion(boolean newStandardConversion) {
		final boolean oldStandardConversion = standardConversion;
		standardConversion = newStandardConversion;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DtoPackage.DTO_BEAN__STANDARD_CONVERSION, oldStandardConversion,
					standardConversion));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#isShared()
	 * @generated
	 */
	@Override
	public boolean isShared() {
		return shared;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#setShared(boolean)
	 * @generated
	 */
	@Override
	public void setShared(boolean newShared) {
		final boolean oldShared = shared;
		shared = newShared;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DtoPackage.DTO_BEAN__SHARED, oldShared, shared));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#isCreatedManually()
	 */
	@Override
	public boolean isCreatedManually() {
		return createdManually;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#setCreatedManually(boolean)
	 */
	@Override
	public void setCreatedManually(boolean newCreatedManually) {
		final boolean oldCreatedManually = createdManually;
		createdManually = newCreatedManually;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, DtoPackage.DTO_BEAN__CREATED_MANUALLY, oldCreatedManually,
					createdManually));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseAdd(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN__ATTRIBUTES:
				return ((InternalEList<InternalEObject>) (InternalEList<?>) getAttributes()).basicAdd(otherEnd, msgs);
		}

		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eInverseRemove(org.eclipse.emf.ecore.InternalEObject, int,
	 * org.eclipse.emf.common.notify.NotificationChain)
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN__ATTRIBUTES:
				return ((InternalEList<?>) getAttributes()).basicRemove(otherEnd, msgs);
		}

		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN__ATTRIBUTES:
				return getAttributes();
			case DtoPackage.DTO_BEAN__STANDARD_CONVERSION:
				return isStandardConversion();
			case DtoPackage.DTO_BEAN__SHARED:
				return isShared();
			case DtoPackage.DTO_BEAN__CREATED_MANUALLY:
				return isCreatedManually();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN__ATTRIBUTES:
				getAttributes().clear();
				getAttributes().addAll((Collection<? extends DTOBeanAttribute>) newValue);
				return;
			case DtoPackage.DTO_BEAN__STANDARD_CONVERSION:
				setStandardConversion((Boolean) newValue);
				return;
			case DtoPackage.DTO_BEAN__SHARED:
				setShared((Boolean) newValue);
				return;
			case DtoPackage.DTO_BEAN__CREATED_MANUALLY:
				setCreatedManually((Boolean) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN__ATTRIBUTES:
				getAttributes().clear();
				return;
			case DtoPackage.DTO_BEAN__STANDARD_CONVERSION:
				setStandardConversion(STANDARD_CONVERSION_EDEFAULT);
				return;
			case DtoPackage.DTO_BEAN__SHARED:
				setShared(SHARED_EDEFAULT);
				return;
			case DtoPackage.DTO_BEAN__CREATED_MANUALLY:
				setCreatedManually(CREATED_MANUALLY_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.mapping.impl.MappingObjectImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case DtoPackage.DTO_BEAN__ATTRIBUTES:
				return attributes != null && !attributes.isEmpty();
			case DtoPackage.DTO_BEAN__STANDARD_CONVERSION:
				return standardConversion != STANDARD_CONVERSION_EDEFAULT;
			case DtoPackage.DTO_BEAN__SHARED:
				return shared != SHARED_EDEFAULT;
			case DtoPackage.DTO_BEAN__CREATED_MANUALLY:
				return createdManually != CREATED_MANUALLY_EDEFAULT;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (standardConversion: ");
		result.append(standardConversion);
		result.append(", shared: ");
		result.append(shared);
		result.append(", createdManually: ");
		result.append(createdManually);
		result.append(')');

		return result.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#getPKAttribute()
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute getPKAttribute() {
		final DomainAttribute pkAttr = getDomainObject().getPKAttribute();

		if (pkAttr == null)
			return null;

		for (final DTOBeanAttribute attr : getAttributes())
			if (attr.getDomainAttribute() != null && attr.getAssociation() == null && attr.getDomainAttribute().equals(pkAttr))
				return attr;

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#getDisplayAttribute()
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute getDisplayAttribute() {
		final DomainAttribute displayAttribute = this.getDomainObject().getDisplayAttribute();

		if (displayAttribute == null)
			return null;

		for (final DTOBeanAttribute attr : getAttributes())
			if (attr.getDomainAttribute() != null && attr.getAssociation() == null
					&& attr.getDomainAttribute().equals(displayAttribute))
				return attr;

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#getModelClassName()
	 * @generated not
	 */
	@Override
	public String getModelClassName() {
		if (getNamespace().getProject().isBoundaryMode())
			return getName();

		return getDomainObject().getName();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#isVirtual()
	 * @generated not
	 */
	@Override
	public boolean isVirtual() {
		final Project project = getNamespace().getProject();
		final DTOBean appLogOnDTO = project.getApplicationLogOnDTO();

		// A data transfer object is never virtual if the boundary mode is set!
		if (project.isBoundaryMode())
			return false;

		// A manually created data transfer object is not virtual!
		if (isCreatedManually())
			return false;

		// Test if this DTO represent the "ApplicationLogOnDTO"
		if (appLogOnDTO != null && appLogOnDTO.equals(this))
			return false;

		// Test if this DTO represents a role used by "ApplicationLogOnDTO"
		if (appLogOnDTO != null && getDomainObject().getTag() == DomainTagEnumeration.ROLE)
			for (final DTOBeanAttribute attr : appLogOnDTO.getAttributes())
				if (attr.getAssociation() != null && attr.getAssociation().getTag() == AssociationTagEnumeration.USER_ROLE
						&& attr.getReferencedDTOBean() != null && attr.getReferencedDTOBean().equals(this))
					return false;

		// The DTO is not virtual if a facade method is generated that really must return the DTO!
		for (final BoundaryBean boundary : project.getAllBoundariesOfProject())
			for (final BoundaryMethod method : boundary.getBoundaryMethods())
				if (method.getReturnType().equals(this) && method.useDTOReturnType())
					return false;

		// A data transfer object is not virtual if it is referenced by an integration method!
		return !isUsedByIntegrationMethod();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#searchAttribute(net.codecadenza.eclipse.model.domain.DomainAttribute,
	 * net.codecadenza.eclipse.model.domain.AbstractDomainAssociation, java.util.List)
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute searchAttribute(DomainAttribute domainAttribute, AbstractDomainAssociation assoc,
			List<AbstractDomainAssociation> assocList) {
		for (final DTOBeanAttribute attr : getAttributes()) {
			if (attr.getDomainAttribute() == null || !attr.getDomainAttribute().equals(domainAttribute))
				continue;

			if (attr.getAssociation() == null && assoc == null)
				return attr;

			if (attr.getAssociation() != null && attr.getAssociation().equals(assoc)) {
				// Note that getAssociationList() won't return null!
				if (attr.getAssociationList().isEmpty() && assocList == null)
					return attr;

				if (assocList.equals(attr.getAssociationList()))
					return attr;
			}
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#searchAttribute(net.codecadenza.eclipse.model.dto.DTOBean,
	 * net.codecadenza.eclipse.model.domain.AbstractDomainAssociation, java.util.List)
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute searchAttribute(DTOBean listDTO, AbstractDomainAssociation assoc,
			List<AbstractDomainAssociation> assocList) {
		for (final DTOBeanAttribute attr : getAttributes()) {
			if (attr.getDomainAttribute() != null || attr.getReferencedDTOBean() == null)
				continue;

			// The association attribute must be set if a DTO attribute references another DTO!
			if (attr.getAssociation() == null || assoc == null)
				continue;

			if (!attr.getReferencedDTOBean().equals(listDTO))
				continue;

			if (attr.getAssociation().equals(assoc)) {
				// Note that getAssociationList() won't return null!
				if (attr.getAssociationList().isEmpty() && assocList == null)
					return attr;

				if (assocList.equals(attr.getAssociationList()))
					return attr;
			}
		}

		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#addJAXBAnnotations()
	 * @generated not
	 */
	@Override
	public boolean addJAXBAnnotations() {
		final Project project = getNamespace().getProject();

		if (project.getMappingStrategy() == MappingAnnotationStrategy.ALWAYS)
			return true;

		if (project.getMappingStrategy() == MappingAnnotationStrategy.NEVER)
			return false;

		if (project.isJavaSEApplication())
			return false;

		for (final IntegrationModule module : project.getIntegrationModules()) {
			// For JMS, Kafka and RMI no JAXB annotations need to be added!
			if (module.getTechnology() != IntegrationTechnology.SOAP && module.getTechnology() != IntegrationTechnology.REST)
				continue;

			// Iterate over all integration beans
			for (final JavaType type : module.getNamespace().getJavaTypes()) {
				final var integrationBean = (AbstractIntegrationBean) type;

				for (final AbstractIntegrationMethod method : integrationBean.getMethods()) {
					// If this DTO is used either as a return or a parameter type the DTO must be annotated!
					if (method.getReturnType().equals(this))
						return true;

					for (final MethodParameter param : method.getMethodParameters())
						if (param.getType().equals(this))
							return true;

					// It might be also the case that this DTO is referenced by another DTO that in turn must be annotated!
					if (method.getReturnType() instanceof final DTOBean dto)
						for (final DTOBeanAttribute attr : dto.getAttributes())
							if (attr.getReferencedDTOBean() != null && attr.getReferencedDTOBean().equals(this))
								return dto.addJAXBAnnotations();

					for (final MethodParameter param : method.getMethodParameters())
						if (param.getType() instanceof final DTOBean dto)
							for (final DTOBeanAttribute attr : dto.getAttributes())
								if (attr.getReferencedDTOBean() != null && attr.getReferencedDTOBean().equals(this))
									return dto.addJAXBAnnotations();
				}
			}
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#isUsedByIntegrationMethod()
	 * @generated not
	 */
	@Override
	public boolean isUsedByIntegrationMethod() {
		for (final IntegrationModule module : this.getNamespace().getProject().getIntegrationModules())
			for (final JavaType type : module.getNamespace().getJavaTypes()) {
				final var integrationBean = (AbstractIntegrationBean) type;

				for (final AbstractIntegrationMethod method : integrationBean.getMethods()) {
					if (method.getReturnType().equals(this))
						return true;

					for (final MethodParameter param : method.getMethodParameters())
						if (param.getType().equals(this))
							return true;

					if (method.getReturnType() instanceof final DTOBean dto)
						for (final DTOBeanAttribute attr : dto.getAttributes())
							if (attr.getReferencedDTOBean() != null && attr.getReferencedDTOBean().equals(this))
								return true;

					for (final MethodParameter param : method.getMethodParameters())
						if (param.getType() instanceof final DTOBean dto)
							for (final DTOBeanAttribute attr : dto.getAttributes())
								if (attr.getReferencedDTOBean() != null && attr.getReferencedDTOBean().equals(this))
									return true;
				}
			}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#addAttribute(net.codecadenza.eclipse.model.domain.DomainAttribute, boolean)
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute addAttribute(DomainAttribute domainAttribute, boolean useExisting) {
		return addAttribute(domainAttribute, null, Collections.emptyList(), useExisting);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#addAttribute(net.codecadenza.eclipse.model.domain.DomainAttribute,
	 * net.codecadenza.eclipse.model.domain.AbstractDomainAssociation, boolean)
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute addAttribute(DomainAttribute domainAttribute, AbstractDomainAssociation assoc, boolean useExisting) {
		return addAttribute(domainAttribute, Arrays.asList(assoc), useExisting);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#addAttribute(net.codecadenza.eclipse.model.domain.DomainAttribute,
	 * java.util.List, boolean)
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute addAttribute(DomainAttribute domainAttribute, List<AbstractDomainAssociation> assocList,
			boolean useExisting) {
		return addAttribute(domainAttribute, null, assocList, useExisting);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#addAttribute(net.codecadenza.eclipse.model.domain.DomainAttribute,
	 * java.lang.String, java.util.List, boolean)
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute addAttribute(DomainAttribute domainAttribute, String name, List<AbstractDomainAssociation> assocList,
			boolean useExisting) {
		final DTOBeanAttribute dtoAttribute = DtoFactory.eINSTANCE.createDTOBeanAttribute();
		AbstractDomainAssociation assoc = null;

		if (assocList != null && !assocList.isEmpty()) {
			assoc = assocList.get(0);

			// Do not add full association list! Just add all elements after the first one!
			if (assocList.size() > 1)
				for (int i = 1; i < assocList.size(); i++)
					dtoAttribute.getAssociationList().add(assocList.get(i));
		}

		if (name == null || name.isEmpty()) {
			dtoAttribute.setName(domainAttribute.getName());

			if (assoc != null) {
				if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation)
					dtoAttribute.setName(assoc.getName() + domainAttribute.getUpperCaseName());
				else
					dtoAttribute.setName(assoc.getTarget().getLowerCaseName() + domainAttribute.getUpperCaseName());
			}
		}
		else
			dtoAttribute.setName(name);

		dtoAttribute.setDomainAttribute(domainAttribute);
		dtoAttribute.setAssociation(assoc);

		if (useExisting) {
			final DTOBeanAttribute existingAttribute = searchAttribute(domainAttribute, assoc, dtoAttribute.getAssociationList());

			if (existingAttribute != null)
				return existingAttribute;
		}

		// Check if an attribute with the same name already exists!
		for (final DTOBeanAttribute existingAttribute : this.getAttributes())
			if (existingAttribute.getName().equals(dtoAttribute.getName()))
				throw new IllegalStateException("A DTO attribute with the name '" + dtoAttribute.getName() + "' already exists!");

		dtoAttribute.setDTOBean(this);
		this.getAttributes().add(dtoAttribute);

		return dtoAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#addAttribute(net.codecadenza.eclipse.model.dto.DTOBean,
	 * net.codecadenza.eclipse.model.domain.AbstractDomainAssociation, boolean)
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute addAttribute(DTOBean referencedDTO, AbstractDomainAssociation assoc, boolean useExisting) {
		return addAttribute(referencedDTO, Arrays.asList(assoc), useExisting);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#addAttribute(net.codecadenza.eclipse.model.dto.DTOBean, java.util.List,
	 * boolean)
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute addAttribute(DTOBean referencedDTO, List<AbstractDomainAssociation> assocList, boolean useExisting) {
		return addAttribute(referencedDTO, null, assocList, useExisting);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#addAttribute(net.codecadenza.eclipse.model.dto.DTOBean, java.lang.String,
	 * java.util.List, boolean)
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute addAttribute(DTOBean referencedDTO, String name, List<AbstractDomainAssociation> assocList,
			boolean useExisting) {
		final DTOBeanAttribute dtoAttribute = DtoFactory.eINSTANCE.createDTOBeanAttribute();

		if (assocList == null || assocList.isEmpty())
			throw new IllegalStateException("The parameter 'assocList' must not be null or empty!");

		final AbstractDomainAssociation assoc = assocList.get(0);

		dtoAttribute.setAssociation(assoc);
		dtoAttribute.setReferencedDTOBean(referencedDTO);

		if (name == null || name.isEmpty()) {
			if (assocList.size() > 1) {
				if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation)
					dtoAttribute.setName(assocList.get(assocList.size() - 1).getName() + assoc.getUpperCaseName());
				else
					dtoAttribute.setName(assocList.get(assocList.size() - 1).getTarget().getLowerCaseName() + assoc.getUpperCaseName());
			}
			else
				dtoAttribute.setName(assoc.getName());
		}
		else
			dtoAttribute.setName(name);

		// Do not add full association list! Just add all elements after the first one!
		if (assocList.size() > 1)
			for (int i = 1; i < assocList.size(); i++)
				dtoAttribute.getAssociationList().add(assocList.get(i));

		if (useExisting) {
			final DTOBeanAttribute existingAttribute = searchAttribute(referencedDTO, assoc, dtoAttribute.getAssociationList());

			if (existingAttribute != null)
				return existingAttribute;
		}

		// Check if an attribute with the same name already exists!
		for (final DTOBeanAttribute existingAttribute : this.getAttributes())
			if (existingAttribute.getName().equals(dtoAttribute.getName()))
				throw new IllegalStateException("A DTO attribute with the name '" + dtoAttribute.getName() + "' already exists!");

		dtoAttribute.setDTOBean(this);
		this.getAttributes().add(dtoAttribute);

		return dtoAttribute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#getClientPKAttribute()
	 * @generated not
	 */
	@Override
	public DTOBeanAttribute getClientPKAttribute() {
		final DomainObject clientDomainObj = getNamespace().getProject().getDomainObjectByTag(DomainTagEnumeration.CLIENT);

		for (final DTOBeanAttribute attr : getAttributes()) {
			if (attr.getAssociation() == null || attr.getDomainAttribute() == null || !attr.getDomainAttribute().isPk())
				continue;

			if (attr.getAssociation().getTag() != AssociationTagEnumeration.CLIENT_REFERENCE)
				continue;

			if (!attr.getAssociation().getTarget().equals(clientDomainObj))
				continue;

			return attr;
		}

		throw new IllegalStateException("The client ID attribute could not be found!");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.dto.DTOBean#getTypeScriptSourceFile()
	 * @generated not
	 */
	@Override
	public WorkspaceFile getTypeScriptSourceFile() {
		final Project project = getNamespace().getProject();

		if (!project.hasAngularClient())
			return null;

		final var path = ANGULAR_DOMAIN_FOLDER + "/" + name.toLowerCase() + ".interface.ts";

		return new WorkspaceFile(project, BuildArtifactType.GUI, path, null);
	}

}
