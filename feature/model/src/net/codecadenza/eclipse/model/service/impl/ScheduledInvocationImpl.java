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
package net.codecadenza.eclipse.model.service.impl;

import net.codecadenza.eclipse.model.service.ScheduledInvocation;
import net.codecadenza.eclipse.model.service.ServicePackage;
import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

/**
 * An implementation of the model object '<em><b>Scheduled Invocation</b></em>'.
 * <p>
 * The following features are implemented:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl#getSecond <em>Second</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl#getMinute <em>Minute</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl#getHour <em>Hour</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl#getDayOfWeek <em>Day Of Week</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl#getDayOfMonth <em>Day Of Month</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl#getMonth <em>Month</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.service.impl.ScheduledInvocationImpl#getYear <em>Year</em>}</li>
 * </ul>
 * </p>
 * @generated
 */
public class ScheduledInvocationImpl extends MethodInvocationImpl implements ScheduledInvocation {
	/**
	 * The default value of the '{@link #getSecond() <em>Second</em>}' attribute
	 * @see #getSecond()
	 * @generated
	 * @ordered
	 */
	protected static final String SECOND_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getSecond() <em>Second</em>}' attribute
	 * @see #getSecond()
	 * @generated
	 * @ordered
	 */
	protected String second = SECOND_EDEFAULT;

	/**
	 * The default value of the '{@link #getMinute() <em>Minute</em>}' attribute
	 * @see #getMinute()
	 * @generated
	 * @ordered
	 */
	protected static final String MINUTE_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMinute() <em>Minute</em>}' attribute
	 * @see #getMinute()
	 * @generated
	 * @ordered
	 */
	protected String minute = MINUTE_EDEFAULT;

	/**
	 * The default value of the '{@link #getHour() <em>Hour</em>}' attribute
	 * @see #getHour()
	 * @generated
	 * @ordered
	 */
	protected static final String HOUR_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getHour() <em>Hour</em>}' attribute
	 * @see #getHour()
	 * @generated
	 * @ordered
	 */
	protected String hour = HOUR_EDEFAULT;

	/**
	 * The default value of the '{@link #getDayOfWeek() <em>Day Of Week</em>}' attribute
	 * @see #getDayOfWeek()
	 * @generated
	 * @ordered
	 */
	protected static final String DAY_OF_WEEK_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDayOfWeek() <em>Day Of Week</em>}' attribute
	 * @see #getDayOfWeek()
	 * @generated
	 * @ordered
	 */
	protected String dayOfWeek = DAY_OF_WEEK_EDEFAULT;

	/**
	 * The default value of the '{@link #getDayOfMonth() <em>Day Of Month</em>}' attribute
	 * @see #getDayOfMonth()
	 * @generated
	 * @ordered
	 */
	protected static final String DAY_OF_MONTH_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getDayOfMonth() <em>Day Of Month</em>}' attribute
	 * @see #getDayOfMonth()
	 * @generated
	 * @ordered
	 */
	protected String dayOfMonth = DAY_OF_MONTH_EDEFAULT;

	/**
	 * The default value of the '{@link #getMonth() <em>Month</em>}' attribute
	 * @see #getMonth()
	 * @generated
	 * @ordered
	 */
	protected static final String MONTH_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getMonth() <em>Month</em>}' attribute
	 * @see #getMonth()
	 * @generated
	 * @ordered
	 */
	protected String month = MONTH_EDEFAULT;

	/**
	 * The default value of the '{@link #getYear() <em>Year</em>}' attribute
	 * @see #getYear()
	 * @generated
	 * @ordered
	 */
	protected static final String YEAR_EDEFAULT = null;

	/**
	 * The cached value of the '{@link #getYear() <em>Year</em>}' attribute
	 * @see #getYear()
	 * @generated
	 * @ordered
	 */
	protected String year = YEAR_EDEFAULT;

	/**
	 * @generated
	 */
	protected ScheduledInvocationImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return ServicePackage.Literals.SCHEDULED_INVOCATION;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getSecond()
	 * @generated
	 */
	@Override
	public String getSecond() {
		return second;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#setSecond(java.lang.String)
	 * @generated
	 */
	@Override
	public void setSecond(String newSecond) {
		final String oldSecond = second;
		second = newSecond;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SCHEDULED_INVOCATION__SECOND, oldSecond, second));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getMinute()
	 * @generated
	 */
	@Override
	public String getMinute() {
		return minute;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#setMinute(java.lang.String)
	 * @generated
	 */
	@Override
	public void setMinute(String newMinute) {
		final String oldMinute = minute;
		minute = newMinute;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SCHEDULED_INVOCATION__MINUTE, oldMinute, minute));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getHour()
	 * @generated
	 */
	@Override
	public String getHour() {
		return hour;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#setHour(java.lang.String)
	 * @generated
	 */
	@Override
	public void setHour(String newHour) {
		final String oldHour = hour;
		hour = newHour;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SCHEDULED_INVOCATION__HOUR, oldHour, hour));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getDayOfWeek()
	 * @generated
	 */
	@Override
	public String getDayOfWeek() {
		return dayOfWeek;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#setDayOfWeek(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDayOfWeek(String newDayOfWeek) {
		final String oldDayOfWeek = dayOfWeek;
		dayOfWeek = newDayOfWeek;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SCHEDULED_INVOCATION__DAY_OF_WEEK, oldDayOfWeek,
					dayOfWeek));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getDayOfMonth()
	 * @generated
	 */
	@Override
	public String getDayOfMonth() {
		return dayOfMonth;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#setDayOfMonth(java.lang.String)
	 * @generated
	 */
	@Override
	public void setDayOfMonth(String newDayOfMonth) {
		final String oldDayOfMonth = dayOfMonth;
		dayOfMonth = newDayOfMonth;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SCHEDULED_INVOCATION__DAY_OF_MONTH, oldDayOfMonth,
					dayOfMonth));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getMonth()
	 * @generated
	 */
	@Override
	public String getMonth() {
		return month;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#setMonth(java.lang.String)
	 * @generated
	 */
	@Override
	public void setMonth(String newMonth) {
		final String oldMonth = month;
		month = newMonth;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SCHEDULED_INVOCATION__MONTH, oldMonth, month));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#getYear()
	 * @generated
	 */
	@Override
	public String getYear() {
		return year;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.ScheduledInvocation#setYear(java.lang.String)
	 * @generated
	 */
	@Override
	public void setYear(String newYear) {
		final String oldYear = year;
		year = newYear;

		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, ServicePackage.SCHEDULED_INVOCATION__YEAR, oldYear, year));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl#eGet(int, boolean, boolean)
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case ServicePackage.SCHEDULED_INVOCATION__SECOND:
				return getSecond();
			case ServicePackage.SCHEDULED_INVOCATION__MINUTE:
				return getMinute();
			case ServicePackage.SCHEDULED_INVOCATION__HOUR:
				return getHour();
			case ServicePackage.SCHEDULED_INVOCATION__DAY_OF_WEEK:
				return getDayOfWeek();
			case ServicePackage.SCHEDULED_INVOCATION__DAY_OF_MONTH:
				return getDayOfMonth();
			case ServicePackage.SCHEDULED_INVOCATION__MONTH:
				return getMonth();
			case ServicePackage.SCHEDULED_INVOCATION__YEAR:
				return getYear();
		}

		return super.eGet(featureID, resolve, coreType);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl#eSet(int, java.lang.Object)
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case ServicePackage.SCHEDULED_INVOCATION__SECOND:
				setSecond((String) newValue);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__MINUTE:
				setMinute((String) newValue);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__HOUR:
				setHour((String) newValue);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__DAY_OF_WEEK:
				setDayOfWeek((String) newValue);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__DAY_OF_MONTH:
				setDayOfMonth((String) newValue);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__MONTH:
				setMonth((String) newValue);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__YEAR:
				setYear((String) newValue);
				return;
		}

		super.eSet(featureID, newValue);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl#eUnset(int)
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case ServicePackage.SCHEDULED_INVOCATION__SECOND:
				setSecond(SECOND_EDEFAULT);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__MINUTE:
				setMinute(MINUTE_EDEFAULT);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__HOUR:
				setHour(HOUR_EDEFAULT);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__DAY_OF_WEEK:
				setDayOfWeek(DAY_OF_WEEK_EDEFAULT);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__DAY_OF_MONTH:
				setDayOfMonth(DAY_OF_MONTH_EDEFAULT);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__MONTH:
				setMonth(MONTH_EDEFAULT);
				return;
			case ServicePackage.SCHEDULED_INVOCATION__YEAR:
				setYear(YEAR_EDEFAULT);
				return;
		}

		super.eUnset(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.service.impl.MethodInvocationImpl#eIsSet(int)
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case ServicePackage.SCHEDULED_INVOCATION__SECOND:
				return second != null;
			case ServicePackage.SCHEDULED_INVOCATION__MINUTE:
				return minute != null;
			case ServicePackage.SCHEDULED_INVOCATION__HOUR:
				return hour != null;
			case ServicePackage.SCHEDULED_INVOCATION__DAY_OF_WEEK:
				return dayOfWeek != null;
			case ServicePackage.SCHEDULED_INVOCATION__DAY_OF_MONTH:
				return dayOfMonth != null;
			case ServicePackage.SCHEDULED_INVOCATION__MONTH:
				return month != null;
			case ServicePackage.SCHEDULED_INVOCATION__YEAR:
				return year != null;
		}

		return super.eIsSet(featureID);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.impl.BasicEObjectImpl#toString()
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy())
			return super.toString();

		final var result = new StringBuilder(super.toString());
		result.append(" (second: ");
		result.append(second);
		result.append(", minute: ");
		result.append(minute);
		result.append(", hour: ");
		result.append(hour);
		result.append(", dayOfWeek: ");
		result.append(dayOfWeek);
		result.append(", dayOfMonth: ");
		result.append(dayOfMonth);
		result.append(", month: ");
		result.append(month);
		result.append(", year: ");
		result.append(year);
		result.append(')');

		return result.toString();
	}

}
